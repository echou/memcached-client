#ifndef TERM_DATA_HPP
#define TERM_DATA_HPP

#include <erl_driver.h>
#include <ei.h>

#include <vector>
#include <stack>

using namespace std;

class TermData 
{
public:
    typedef ErlDrvTermData Item;

    TermData(bool tuple=false)
    {
        if (tuple) open_tuple();
    }

    void add_atom(char* atom) 
    {
        inc_counter();
        add(ERL_DRV_ATOM, driver_mk_atom(atom));
    }

    void add_uint(uint32_t uint)
    {
        inc_counter();
        add(ERL_DRV_UINT, (Item)uint);
    }

    void add_port(ErlDrvPort port)
    {
        inc_counter();
        add(ERL_DRV_PORT, driver_mk_port(port));
    }

    void add_buf(const char* buf, size_t size, bool copy=false) 
    {
        inc_counter();

        if (!copy) 
        {
            add(ERL_DRV_BUF2BINARY, (Item)buf, (Item)size);
        }
        else 
        {
            ErlDrvBinary * bin = driver_alloc_binary(size);
            memcpy(bin->orig_bytes, buf, size);
            add(ERL_DRV_BINARY, (Item)bin, (Item)size, (Item)0);
        }
    }

    void open_tuple()
    {
        inc_counter();
        stk.push(make_pair(0, 't'));
    }

    void close_tuple(bool check=true)
    {
        if (check && (stk.empty() || stk.top().second != 't'))
            return;
        add(ERL_DRV_TUPLE, (Item)stk.top().first);
        stk.pop();
    }

    void open_list()
    {
        inc_counter();
        stk.push(make_pair(0, 'l'));
    }

    void close_list(bool check=true)
    {
        if (check && (stk.empty() || stk.top().second != 'l'))
            return;
        add(ERL_DRV_NIL, ERL_DRV_LIST, stk.top().first+1);
        stk.pop();
    }

    void flush()
    {
        while (!stk.empty())
        {
            switch (stk.top().second)
            {
            case 'l':
                close_list(false);
                break;
            case 't':
                close_tuple(false);
                break;
            default:
                return;
            }
        }
    }

    int output(ErlDrvPort port, Item to=0)
    {
        flush();
        if (!spec.empty())
        {
            if (to)
                return driver_send_term(port, to, &(spec.begin()[0]), spec.size());
            else
                return driver_output_term(port, &(spec.begin()[0]), spec.size());
        }
        return 0;
    }

private:
    inline void inc_counter() { if (!stk.empty()) stk.top().first++; }
    inline void add(Item a) { spec.push_back(a); }
    inline void add(Item a, Item b) { spec.push_back(a); spec.push_back(b); }
    inline void add(Item a, Item b, Item c) { spec.push_back(a); spec.push_back(b); spec.push_back(c); }
    inline void add(Item a, Item b, Item c, Item d) { spec.push_back(a); spec.push_back(b); spec.push_back(c); spec.push_back(d); }
private:
    vector<Item> spec;
    stack<pair<int, char> > stk;
};

class IOVec
{
public:
    IOVec(SysIOVec* vec, size_t vlen): m_vec(vec), m_vlen(vlen), vidx(0) 
    {
        cptr = m_vec[vidx].iov_base;
        walk(0); // skip the first (nil, 0) vectors
    }

    IOVec(char* buf, size_t len): m_vec(&m_default_vec), m_vlen(1), vidx(0) 
    {
        m_default_vec.iov_base = buf;
        m_default_vec.iov_len = len;
        cptr = buf;
    }

    bool get(char& c) 
    {
        char *p = walk(1);
        if (p) c = *p;
        return p!=NULL;
    }

    // 16 bit
    bool get(short& s) 
    {
        char *p = walk(2);
        if (p) s = ntohs(*(short*)p);
        return p != NULL;
    }

    // 32 bit
    bool get(int& i) 
    {
        char *p = walk(4);
        if (p) i = ntohl(*(int*)p);
        return p != NULL;
    }

    // 32 bit
    bool get(unsigned int& i) 
    {
        char *p = walk(4);
        if (p) i = (unsigned int)ntohl(*(int*)p);
        return p != NULL;
    }


#if __LP64__
    // size_t is 64bit on 64bit machines
    bool get(size_t& i)
    {
        char *p = walk(4);
        if (p) i = (size_t)ntohl(*(int*)p);
        return p != NULL;
    }
#endif

    bool get(char*& buf, size_t count) // the buffer cannot cross IOVec border.
    {
        buf = walk(count);
        return buf != NULL;
    }

private:
    char* walk(size_t count) 
    {
        if (vidx == m_vlen) // reach the end of vectors.
            return NULL;

        char * ret = NULL;
        size_t left = m_vec[vidx].iov_len - (cptr - m_vec[vidx].iov_base);
        if (left == count) // walk to next vec
        {
            ret = cptr;
            vidx++;
            while (vidx < m_vlen && m_vec[vidx].iov_len == 0) vidx++;
            if (vidx < m_vlen)
                cptr = m_vec[vidx].iov_base;
        } else if (left > count) { // stay at current vector
            ret = cptr;
            cptr += count;
        }
        return ret;
    }

private:
    SysIOVec m_default_vec; // emulate one buffer (giving buf ptr and len)

    SysIOVec* m_vec;
    size_t m_vlen;


    char * cptr; // current pointer in one vec.
    size_t vidx; // vector index
};

#endif

