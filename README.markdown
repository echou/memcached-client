## Description

mcache is an erlang memcached client application. It utilizes many new features to 
improve performance such as NIF (only from R13B03 on), dynamic compiling 
modules.


## Usage

1. Get a single key.
<pre>
mcache:get(Class, Key).
</pre>
   
For example: <code>mcache:get(my.friends, foobar)</code> gets the key <code>"my.friends:foobar"</code>
   
Return values:

- <code>undefined</code>, if key not found.
- <code>Value</code>, any other values.

2. Get multiple keys.
<pre>
mcache:mget(Class, [Key|_]).
</pre>

Note: it gets all the keys with the same <code>Class</code>. 

3. Set a key and value.
<pre>
mcache:set(Class, Key, Value, Format, Expiry)
</pre>

Format can be the following atoms:
- <code>raw</code>, as iolist.
- <code>native</code>, term_to_binary()
- <code>json</code>, convert to json string (using an enhanced version of EEP0018)
- <code>int</code>, data in <<Int:32>> format.

Expiry can be as follows:
- <code>default</code>, uses ExpireConfig
- <code>infinity</code>, no expiration
- <code>{X, seconds}</code>, or minutes, hours, days, etc.
- <code>Integer</code>, any numeric seconds.

This argument can be ignored. <code>default</code> is used in this case.