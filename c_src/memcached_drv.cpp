#include <string>
#include <netinet/in.h>

#include <libmemcached/memcached.h>

#include "termdata.hpp"

#include <iostream>

using namespace std;

#define CMD_SET_SERVERS 0

#define CMD_GET     1
#define CMD_MGET    2
#define CMD_SET     3
#define CMD_DELETE  4
#define CMD_MGET2   5 
#define CMD_SET_NOREPLY    13
#define CMD_DELETE_NOREPLY 14

class Cache 
{
public:
    Cache() : mc(memcached_create(NULL)) {}
    ~Cache() { memcached_free(mc); }

    operator memcached_st*() { return mc; }

    bool setServers(int binary, const string& servers)
    {
        memcached_free(mc);
        mc = memcached(servers.c_str(), servers.length());
        memcached_behavior_set(mc, MEMCACHED_BEHAVIOR_KETAMA_WEIGHTED, 1);
        memcached_behavior_set(mc, MEMCACHED_BEHAVIOR_BINARY_PROTOCOL, binary);
        memcached_behavior_set(mc, MEMCACHED_BEHAVIOR_NO_BLOCK, 1);
        memcached_behavior_set(mc, MEMCACHED_BEHAVIOR_TCP_NODELAY, 1);
        return true;
    }

private:
    memcached_st * mc;
};

class Driver 
{
public:
    Driver(ErlDrvPort port): m_port(port)
    {
        set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
    }

    ErlDrvSSizeT control(unsigned int command, char *buf, int len, char **rbuf, int rlen) 
    {
        switch(command) 
        {
        case CMD_SET_SERVERS:
            return doSetServers(buf, len, rbuf, rlen);
        }
        return 0;
    }

    void output(IOVec& vec)
    {
        char cmd;
        uint32_t seq;

        if (vec.get(cmd) && vec.get(seq))
        {
            switch(cmd) 
            {
            case CMD_GET:
                doGet(seq, vec);
                break;
            case CMD_MGET:
                doMGet(seq, vec);
                break;
            case CMD_SET:
                doSet(seq, vec, true);
                break;
            case CMD_SET_NOREPLY:
                doSet(seq, vec, false);
                break;
            case CMD_DELETE:
                doDelete(seq, vec, true);
                break;
            case CMD_DELETE_NOREPLY:
                doDelete(seq, vec, false);
                break;
            case CMD_MGET2:
                doMGet2(seq, vec);
                break;
            }
        }
    }

private:
    
    // some TermData helpers
    TermData createReply(uint32_t seq)
    {
        TermData td;
        td.open_tuple();
        td.add_atom((char*)"mc_async");
        td.add_uint(seq);
        return td;
    }

    TermData& ok(TermData& td, bool in_tuple = true)
    {
        if (in_tuple) td.open_tuple();
        td.add_atom((char*)"ok");
        return td;
    }

    TermData& error(TermData& td, memcached_return rc)
    {
        const char * errmsg = memcached_strerror(m_cache, rc); 
        td.open_tuple();
        td.add_atom((char*)"error");
        td.add_buf((char*)errmsg, strlen(errmsg));
        td.close_tuple();
        return td;
    }

    TermData& badarg(TermData& td)
    {
        td.open_tuple();
        td.add_atom((char*)"error");
        td.add_atom((char*)"badarg");
        td.close_tuple();
        return td;
    }

    void send(TermData& td)
    {
        td.output(m_port, driver_caller(m_port));
    }

    // command handlers

    ErlDrvSSizeT doSetServers(char* buf, int len, char** rbuf, int rlen)
    {
        int binary = (int)(*(char*)buf);
        m_cache.setServers(binary, buf+1);
        return 0;
    }

    void doGet(uint32_t seq, IOVec& vec)
    {
        TermData td = createReply(seq);

        size_t klen;
        char * key;

        // <<KeyLen:32, Key/binary>>
        if (!(vec.get(klen) && vec.get(key, klen)))
        {
            send(badarg(td));
            return;
        }

        size_t vlen;
        uint32_t flags;

        memcached_return rc;
        char* value = memcached_get(m_cache, (const char*)key, klen, &vlen, &flags, &rc);
        if (rc == MEMCACHED_SUCCESS)
        {
            td.open_tuple();
            td.add_atom((char*)"ok");

            td.open_tuple();
            td.add_buf(value, vlen);
            td.add_uint(flags);
            td.close_tuple();

            td.close_tuple();
        }
        else if (rc == MEMCACHED_NOTFOUND)
        {
            ok(td, true);
            td.add_atom((char*)"undefined");
        }
        else
        {
            error(td, rc);
        }

        send(td);
        if (value) free(value);

    }

    void doMGet(uint32_t seq, IOVec& vec)
    {
        TermData td = createReply(seq);

        // <<Count:32, KeyLen:32, Key/binary, ...>>
        int num_keys;
        if (!vec.get(num_keys) || num_keys <= 0 || num_keys>2000)
            goto L_badarg;
	    
        {
            char * keys[num_keys];
            size_t lengths[num_keys];

            for(int i=0;i<num_keys;i++)
            {
                if (!(vec.get(lengths[i]) && vec.get(keys[i], lengths[i])))
                    goto L_badarg;
            }

            goto L_arg_ok;

        L_arg_ok:
            memcached_return rc;
            rc = memcached_mget(m_cache, (const char**)keys, lengths, num_keys);

            if (rc != MEMCACHED_SUCCESS)
            {
                send(error(td, rc));
                return;
            }

            ok(td, true);
            td.open_list();

            vector<memcached_result_st*> free_list;

            // [ {Key, Value, Flag}, ... ]
            memcached_result_st *result;
            while ( (result = memcached_fetch_result(m_cache, NULL, &rc)) ) 
            {
                free_list.push_back(result);

                td.open_tuple();
                td.add_buf(result->item_key, result->key_length);
                td.add_buf(result->value.string, result->value.end - result->value.string);
                td.add_uint(result->item_flags);
                td.close_tuple();
            } 

            td.close_list();
            send(td);
            for(size_t i=0; i<free_list.size(); i++) memcached_result_free(free_list[i]);
            return;
	    }
    L_badarg:
        send(badarg(td));
        return;
    }

    void doMGet2(uint32_t seq, IOVec& vec)
    {
        TermData td = createReply(seq);

        // <<Count:32, KeyLen:32, Key/binary, ...>>
        size_t num_keys;
        if (!vec.get(num_keys) || num_keys == 0 || num_keys>2000)
            goto L_badarg;

        {
            char * keys[num_keys];
            size_t lengths[num_keys];

            for(size_t i=0;i<num_keys;i++)
            {
                if (!(vec.get(lengths[i]) && vec.get(keys[i], lengths[i])))
                    goto L_badarg;
                //printf("key #%d = %s\r\n", i, keys[i]);
            }

            goto L_arg_ok;

        L_arg_ok:
            memcached_return rc;
            rc = memcached_mget(m_cache, (const char**)keys, lengths, num_keys);

            if (rc != MEMCACHED_SUCCESS)
            {
                send(error(td, rc));
                return;
            }

            vector<memcached_result_st*> results;

            // [ {Key, Value, Flag}, ... ]
            memcached_result_st *result;
            while ( (result = memcached_fetch_result(m_cache, NULL, &rc)) ) 
            {
                results.push_back(result);
                //std::cout << "result: " << std::string(result->key, result->key_length) << "\r\n";
            }

            ok(td, true);
            td.open_list();

            int ri = 0;
            for(size_t i=0; i<num_keys; i++)
            {
                memcached_result_st* r = NULL;
                for(size_t j=ri; j<results.size(); j++)
                {
                    if (lengths[i] == results[j]->key_length &&
                        memcmp(keys[i], results[j]->item_key, lengths[i]) == 0)
                    {
                        r = results[j];
                        //ri = j+1;
                        break;
                    }
                }
                if (r)
                {
                    td.open_tuple();
                    //td.add_buf(result->key, result->key_length);
                    td.add_buf(r->value.string, r->value.end-r->value.string);
                    td.add_uint(r->item_flags);
                    td.close_tuple();
                }
                else
                {
                    td.add_atom((char*)"undefined");
                }
            }

            td.close_list();
            send(td);

            for(size_t i=0; i<results.size(); i++) 
    	    memcached_result_free(results[i]);
            return;
        }
    L_badarg:
        send(badarg(td));
        return;
    }

    void doSet(uint32_t seq, IOVec& vec, bool wantReply)
    {
        TermData td = createReply(seq);
        // <<Seq:32, Op:8, KeyLen:32, VLen:32, Flags:32, Expires:32, Key/binary, Value/binary>>
        char op;

        size_t klen, vlen;
        uint32_t flags, expires;
        char *key, *value;

        if (!(vec.get(op) &&
              vec.get(klen) &&
              vec.get(vlen) &&
              vec.get(flags) &&
              vec.get(expires) &&
              vec.get(key, klen) &&
              vec.get(value, vlen)))
        {
            if (wantReply)
                send(badarg(td));
            return;
        }

        memcached_return rc;
        if (op == 's')
            rc = memcached_set(m_cache, (const char*)key, klen, value, vlen, expires, flags);
        else if (op == 'a')
            rc = memcached_add(m_cache, (const char*)key, klen, value, vlen, expires, flags);
        else if (op == 'r')
            rc = memcached_replace(m_cache, (const char*)key, klen, value, vlen, expires, flags);
        else
            rc = memcached_set(m_cache, (const char*)key, klen, value, vlen, expires, flags);

        if (wantReply) {
            if (rc == MEMCACHED_SUCCESS)
                send(ok(td, false));
            else
                send(error(td, rc));
        }
    }

    void doDelete(uint32_t seq, IOVec& vec, bool wantReply)
    {
        TermData td = createReply(seq);

        // <<Expires:32, KeyLen:32, Key/binary>>
        uint32_t expires;
        size_t klen;
        char *key;

        if (!(vec.get(expires) &&
              vec.get(klen) &&
              vec.get(key, klen)))
        {
            if (wantReply)
                send(badarg(td));
            return;
        }

        memcached_return rc;
        rc = memcached_delete(m_cache, (const char*)key, klen, expires);
        if (wantReply) {
            if (rc == MEMCACHED_SUCCESS)
                send(ok(td, false));
            else
                send(error(td, rc));
        }
    }

private:
    ErlDrvPort m_port;
    Cache m_cache;
};


////////////////////////////////////////
//      DRIVER CALLBACKS
////////////////////////////////////////

static ErlDrvData driverStart(ErlDrvPort port, char *buff)
{
    return (ErlDrvData)(new Driver(port));
}

static void driverStop(ErlDrvData handle)
{
    delete (Driver*)handle;
}

static ErlDrvSSizeT driverControl(ErlDrvData drv_data, unsigned int command, char *buf, ErlDrvSizeT len, char **rbuf, ErlDrvSizeT rlen) 
{
    return ((Driver*)drv_data)->control(command, buf, len, rbuf, rlen);
}

#if 0
static void driverOutput(ErlDrvData drv_data, char* buf, int len)
{
    IOVec vec(buf, len);
    ((Driver*)drv_data)->output(vec);
}
#endif

static void driverOutputv(ErlDrvData drv_data, ErlIOVec* ev)
{
    /*
    printf("outputv=%p, sys_iov=%p, n=%d\r\n", ev, ev->iov, ev->vsize);
    for(int i=0; i<ev->vsize; i++)
        printf("  iov[%d]=(%p, %d)\r\n", i, ev->iov[i].iov_base, ev->iov[i].iov_len);
    */

    IOVec vec(ev->iov+1, ev->vsize-1); //TODO: the first iov seems to be (nil, 0)
    ((Driver*)drv_data)->output(vec);
}


ErlDrvEntry driver_entry = {
   NULL,                    /* F_PTR init, N/A */
   driverStart,         /* L_PTR start, called when port is opened */
   driverStop,          /* F_PTR stop, called when port is closed */
   NULL, //driverOutput,        /* F_PTR output, called when erlang has sent */
   NULL,                    /* F_PTR ready_input, called when input descriptor ready */
   NULL,                    /* F_PTR ready_output, called when output descriptor ready */
   (char*)"memcached_drv",         /* char *driver_name, the argument to open_port */
   NULL,                    /* F_PTR finish, called when unloaded */
   NULL,                    /* handle */
   driverControl,       /* F_PTR control, port_command callback */
   NULL,                    /* F_PTR timeout, reserved */
   driverOutputv,       /* F_PTR outputv, reserved */
   NULL,
   NULL,
   NULL,
   NULL,
   ERL_DRV_EXTENDED_MARKER,
   ERL_DRV_EXTENDED_MAJOR_VERSION,
   ERL_DRV_EXTENDED_MINOR_VERSION,
   ERL_DRV_FLAG_USE_PORT_LOCKING
};

#ifdef __cplusplus
extern "C" {
#endif

DRIVER_INIT(memcached_drv) /* must match name in driver_entry */
{
    return &driver_entry;
}

#ifdef __cplusplus
}
#endif

