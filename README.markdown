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



