#include <string>
#include <netinet/in.h>

#include <libmemcached/memcached.h>

#include "termdata.hpp"

using namespace std;

#define CMD_SET_SERVERS 0
#define CMD_SET 1
#define CMD_MGET 2
#define CMD_MGET_BY_CLASS 3

class Cache 
{
public:
    Cache() : mc(memcached_create(NULL)) {}
    ~Cache() { memcached_free(mc); }

    operator memcached_st*() { return mc; }

    bool setServers(const string& servers)
    {
        memcached_server_st* s = memcached_servers_parse(servers.c_str());
        if (!s)  return false;

        int ret = memcached_server_push(mc, s);
        memcached_server_list_free(s);
        return ret == 0;
        
    }

    void initBehaviors() 
    {
        memcached_behavior_set(mc, MEMCACHED_BEHAVIOR_KETAMA_WEIGHTED, 1);
        memcached_behavior_set(mc, MEMCACHED_BEHAVIOR_BINARY_PROTOCOL, 1);
        memcached_behavior_set(mc, MEMCACHED_BEHAVIOR_NO_BLOCK, 1);
        memcached_behavior_set(mc, MEMCACHED_BEHAVIOR_TCP_NODELAY, 1);
        memcached_behavior_set(mc, MEMCACHED_BEHAVIOR_SORT_HOSTS, 1);
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

    int control(unsigned int command, char *buf, int len, char **rbuf, int rlen) 
    {
        switch(command) {
            case CMD_SET_SERVERS:
                return doSetServers(buf, len, rbuf, rlen);
            default:
                return 0;
        }
    }

    void output(char *buf, int len)
    {
        int cmd = *buf;
        uint32_t seq = ntohl(*(uint32_t*)(buf+1));
        switch(cmd) {
            case CMD_SET:
                doSet(seq, buf+5, len-5);
                break;
            case CMD_MGET:
                doMGet(seq, buf+5, len-5);
                break;
        }
    }

private:
    
    TermData createReply(uint32_t seq, char* atom)
    {
        TermData td;
        td.open_tuple();
        td.add_atom("mc_async");
        td.add_uint(seq);

        td.open_tuple();
        td.add_atom(atom);
        return td;
    }

    void sendError(uint32_t seq, memcached_return rc, bool to_caller)
    {
        TermData td = createReply(seq, "error");
        const char * errmsg = memcached_strerror(m_cache, rc); 
        td.add_buf((char*)errmsg, strlen(errmsg));
        td.output(m_port, to_caller);
    }


    int doSetServers(char* buf, int len, char** rbuf, int rlen)
    {
        m_cache.setServers(buf);
        m_cache.initBehaviors();
        return 0;
    }

    void doSet(uint32_t seq, char* buf, int len)
    {
        // <<KeyLen:32, Key/binary, ValueLen:32, Value/binary, Flags:32, Expires:32>>
        char *p = buf;
        size_t klen = ntohl(*(int*)p); p+=4;
        char *key = p; p += klen;
        size_t vlen = ntohl(*(int*)p); p+=4;
        char *value = p; p += vlen;
        uint32_t flags = ntohl(*(int*)p); p+=4;
        uint32_t expires = ntohl(*(int*)p);

        memcached_return rc;
        rc = memcached_set(m_cache, (const char*)key, klen, value, vlen, expires, flags);
        if (rc == MEMCACHED_SUCCESS)
        {
            TermData td = createReply(seq, "ok");
            td.output(m_port, true);
        }
        else
        {
            sendError(seq, rc, true);
        }
    }

    void doMGet(uint32_t seq, char* buf, int len)
    {
        // <<Count:32, KeyLen:32, Key/binary, ...>>
        char *p = buf;
        int num_keys = ntohl(*(int*)p); p += 4;
        char * keys[num_keys];
        size_t lengths[num_keys];

        for(int i=0;i<num_keys;i++)
        {
            lengths[i] = ntohl(*(int*)p); p+=4;
            keys[i] = p; p += lengths[i];
        }

        memcached_return rc;
        rc = memcached_mget(m_cache, (const char**)keys, lengths, num_keys);

        if (rc != MEMCACHED_SUCCESS)
        {
            sendError(seq, rc, true);
            return;
        }

        TermData td = createReply(seq, "ok");

        vector<memcached_result_st*> free_list;

        // [ {Key, Value, Flag}, ... ]
        td.open_list();
        memcached_result_st *result;
        while ( (result = memcached_fetch_result(m_cache, NULL, &rc)) ) 
        {
            free_list.push_back(result);
            td.open_tuple();
            td.add_buf(result->key, result->key_length);
            td.add_buf(memcached_string_value(&(result->value)), memcached_string_length(&(result->value)));
            td.add_uint(result->flags);
            td.close_tuple();
        } 
        td.close_list();
        td.output(m_port, true);

        for(int i=0; i<free_list.size(); i++) memcached_result_free(free_list[i]);
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

static int driverControl(ErlDrvData drv_data, unsigned int command, char *buf, int len, char **rbuf, int rlen) 
{
    return ((Driver*)drv_data)->control(command, buf, len, rbuf, rlen);
}

static void driverOutput(ErlDrvData drv_data, char* buf, int len)
{
    ((Driver*)drv_data)->output(buf, len);
}


ErlDrvEntry driver_entry = {
   NULL,                    /* F_PTR init, N/A */
   driverStart,         /* L_PTR start, called when port is opened */
   driverStop,          /* F_PTR stop, called when port is closed */
   driverOutput,        /* F_PTR output, called when erlang has sent */
   NULL,                    /* F_PTR ready_input, called when input descriptor ready */
   NULL,                    /* F_PTR ready_output, called when output descriptor ready */
   "memcached_drv",         /* char *driver_name, the argument to open_port */
   NULL,                    /* F_PTR finish, called when unloaded */
   NULL,                    /* handle */
   driverControl,       /* F_PTR control, port_command callback */
   NULL,                    /* F_PTR timeout, reserved */
   NULL,					/* F_PTR outputv, reserved */
   NULL,
   NULL,
   NULL,
   NULL,
   ERL_DRV_EXTENDED_MARKER,
   ERL_DRV_EXTENDED_MAJOR_VERSION,
   ERL_DRV_EXTENDED_MAJOR_VERSION,
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

