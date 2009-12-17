#ifndef TERM_DATA_HPP
#define TERM_DATA_HPP

#include <erl_driver.h>
#include <ei.h>

#include <list>
#include <vector>
#include <stack>

using namespace std;

class TermData 
{
public:

    typedef ErlDrvTermData Item;

    void add_atom(char* atom) 
    {
        spec.push_back(ERL_DRV_ATOM);
        spec.push_back(driver_mk_atom(atom));
        if (!stk.empty()) stk.top().first++;
    }

    void add_uint(uint32_t uint)
    {
        spec.push_back(ERL_DRV_UINT);
        spec.push_back((Item)uint);
        if (!stk.empty()) stk.top().first++;
    }

    void add_buf(char* buf, size_t size, bool copy=false) 
    {
        if (not copy) {
            spec.push_back(ERL_DRV_BUF2BINARY);
            spec.push_back((Item)buf);
            spec.push_back((Item)size);
        }
        else {
            ErlDrvBinary * bin = driver_alloc_binary(size);
            memcpy(bin->orig_bytes, buf, size);
            spec.push_back(ERL_DRV_BINARY);
            spec.push_back((Item)bin);
            spec.push_back((Item)size);
            spec.push_back((Item)0);
        }

        if (!stk.empty()) stk.top().first++;
    }

    void open_tuple()
    {
        if (!stk.empty()) stk.top().first++;
        stk.push(make_pair(0, 't'));
    }

    void close_tuple(bool check=true)
    {
        if (check && (stk.empty() || stk.top().second != 't'))
            return;
        spec.push_back(ERL_DRV_TUPLE);
        spec.push_back(stk.top().first);
        stk.pop();
    }

    void open_list()
    {
        if (!stk.empty()) stk.top().first++;
        stk.push(make_pair(0, 'l'));
    }

    void close_list(bool check=true)
    {
        if (check && (stk.empty() || stk.top().second != 'l'))
            return;
        spec.push_back(ERL_DRV_NIL);
        spec.push_back(ERL_DRV_LIST);
        spec.push_back(stk.top().first+1);
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
            }
        }
    }

    int output(ErlDrvPort port)
    {
        flush();
        if (!spec.empty())
            return driver_output_term(port, &(spec.begin()[0]), spec.size());
        return 0;
    }

    int output(ErlDrvPort port, Item to)
    {
        flush();
        if (!spec.empty())
            return driver_send_term(port, to, &(spec.begin()[0]), spec.size());
        return 0;
    }

    int output(ErlDrvPort port, bool to_caller)
    {
        flush();
        if (!spec.empty())
        {
            if (to_caller)
                return driver_send_term(port, driver_caller(port), &(spec.begin()[0]), spec.size());
            else
                return driver_output_term(port, &(spec.begin()[0]), spec.size());
        }
        return 0;
    }

private:
    vector<Item> spec;
    stack<pair<int, char> > stk;
};


#endif

