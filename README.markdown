## Description

mcache is an erlang memcached client application. It utilizes many new features to 
improve performance such as NIF (only from R13B03 on), dynamic compiling 
modules.

## Start/Stop/Configuration

mcache is an OTP application. You may start or stop it as following:
<pre>
application:start(mcache)
application:stop(mcache).
</pre>

It requires the following configuration (in `-config <ConfigFile>` or `sys.config`)
<pre>
{mcache,
	[
		{pools,[
			[{name, generic}, % Pool name is "generic"
			 {servers, 
				[ 
					{ {1,0,0,1}, 11211, 256 },   % Servers definition. IP address should be in {A,B,C,D} format
					{ {1,0,0,2}, 11211, 256 }
                ]}
			]
		]}
	]
}
</pre>

## Usage

1. Get a single key.
 <pre>
 mcache:get(Class, Key).
 </pre>
 For example: `mcache:get(my.friends, foobar)` gets the key `"my.friends:foobar"`
 
 Which memcached server is selected? The following steps go:
 1. Get expiry config from `Class`.  Default is `{generic, 300}` (i.e. `{PoolName, ExpireSeconds}`)
 1. Get the server continuum from the pool name. (in ketama's consistent hashing algorithm)
 1. Calc the server from Key's MD5 hash value according the above continuum.
   
 **Return values**:
 - `undefined`, if key not found.
 - `Value`, any other values.


1. Get multiple keys.
 <pre>
 mcache:mget(Class, [Key|_]).
 </pre>

 Note: it gets all the keys with the same `Class`. 

1. Set a key and value.
 <pre>
 mcache:set(Class, Key, Value, Format, Expiry)
 </pre>

 **Class** is any atom or iolist.

 **Key** can be any iolist.

 **Format** can be the following atoms:
 - `raw`, an iolist.
 - `native`, any Erlang term (uses `term_to_binary()`)
 - `json`, convert to json string (using an enhanced version of EEP0018)
 - `int`, data in `<<Int:32>>` format.

 **Expiry** can be as following:
 - `default`, uses ExpireConfig
 - `infinity`, no expiration
 - `{X, seconds}`, or minutes, hours, days, etc.
 - `Integer`, any numeric seconds.

 This argument can be ignored. `default` is used in this case.

 **Return** 
 * A list of {Key, Value} pairs. The key doesn't contain `Class` part. 
 * Missing keys **won't** show in the list.
 * If all keys are missing, an empty list ([]) is returned.