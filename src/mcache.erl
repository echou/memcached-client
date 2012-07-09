%
% wraps memcached_drv
%

-module(mcache).
-author('echou327@gmail.com').

-export([start/0, get/2, get_raw/2, mget/2, set/5, delete/2]).
		
-define(DICT, dict).

start() ->
    application:start(mcache).
    
get(Class, Key) ->
    Get = fun() ->
            {Pool, _Expiry} = mcache_expires:expire(Class),
            io:format("~p~n", [{Pool, _Expiry}]),
            {mc_async, 0, {ok, Value}} = memcached_drv:get(Pool, 0, mcache_util:map_key(Class, Key)),
            mcache_util:decode_value(Value)
    end,
    catch Get().

get_raw(Class, Key) ->
    {Pool, _Expiry} = mcache_expires:expire(Class),
    {mc_async, 0, {ok, Value}} = memcached_drv:get(Pool, 0, mcache_util:map_key(Class, Key)),
    Value.

    
mget(Class, [_|_]=Keys) ->
    {Pool, _Expiry} = mcache_expires:expire(Class),
    RealKeys = lists:foldl(fun(K, Acc) ->
                            K1 = mcache_util:map_key(Class, K),
                            [K1|Acc]
                        end, [], Keys),

    {mc_async, 0, {ok, Values}} = memcached_drv:mget(Pool, 0, lists:reverse(RealKeys)),
    mget_zip(Keys, Values, []).

set(Class, Key, Value, Format, Expiry) ->
    {Pool, DefaultExpiry} = mcache_expires:expire(Class),
	{Value1, Flags} = mcache_util:encode_value(Value, Format),
	Expiry1 = mcache_util:encode_expiry(Expiry, DefaultExpiry),
    memcached_drv:set(Pool, 0, set, mcache_util:map_key(Class, Key), Value1, Flags, Expiry1),
    ok.

delete(Class, Key) ->
    {Pool, _} = mcache_expires:expire(Class),
    memcached_drv:delete(Pool, 0, mcache_util:map_key(Class, Key)),
    ok.


mget_zip([], [], L) ->
    lists:reverse(L);
mget_zip([_K|KL], [undefined|VL], L) ->
    mget_zip(KL, VL, L);
mget_zip([K|KL], [{Val, Flags}|VL], L) ->
    mget_zip(KL, VL, [{K, mcache_util:decode_value({Val, Flags})}|L]).
