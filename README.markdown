An efficient memcached client that simply wraps around libmemcached.

# Requirement

libmemcached 1.0+ should be installed. OTP should be R15B or above.

# Build

    ./rebar compile

a `memcached_drv.so` is created under `priv/lib`.

# Usage

**start the application**

````
mcache:start(). 

  or

application:start(mcache).
````

**get**

    mcache:get(Prefix, Key)
    
fetches \<Prefix>:\<Key> from memcached. 


**set**

    mcache:set(Prefix, Key, Value, Type, Timeout)
    
* Value: depends on `Type`, can be binary, iolist, int or erlang term.
* Type: can be one of `raw` (binary), `native` (erlang term, saved in `term_to_binary` format), `int` (an integer).
* Timeout: can be one of `default` (default expiration: 300 seconds), `infinity`, `{X, seconds}`, `{X, minutes}`, `{X, hours}`, `{X, days}`, or an integer (as is).

*Note 1*: no reply is returned for best performance.   
*Note 2*: `Type` is encoded as memcached protocol field `flags`: 0 - `raw`, 1 - `native`, 100 - `int`.


**delete**

    mcache:delete(Prefix, Key)
    
# Configuration

**pools**

````
{pools,  % defines several pools
 	[{name,generic}, % pool name
 	 {connection_count,10},
 	 {servers,[{"127.0.0.1:11211",255},{"192.168.1.1:11211",255}]}] % servers and weights
 	% another pool definition, ....
}
````

**expires**

````
{expires, 
  [ {prefix1, {PoolName, Timeout1}},
    {prefix2, {PoolName, Timeout2}} ]
}
````

* `PoolName`: is one of the pool names from pools definitions.
* `Timeout`: sees the timeout definition of `mcache:set` function.