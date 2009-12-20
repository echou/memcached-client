%
% wraps memcached_drv
%

-module(mcache2).
-author('echou327@gmail.com').

-export([get/2, mget/2, set/5, delete/3]).

get(Class, Key) ->
    {Pool, _Expiry} = mcache_expires:expire(Class),
    {mc_async, 0, {ok, Value}} = memcached_drv:get(Pool, 0, {Class, Key}),
    mcache_util:decode_value(Value).
    
mget(Class, [_|_]=Keys) ->
    {Pool, _Expiry} = mcache_expires:expire(Class),
    KeyDict = lists:foldl(fun(K, D) ->
                            K1 = mcache_util:map_key(Class, K),
                            dict:store(K1, K, D)
                        end, dict:new(), Keys),

    {mc_async, 0, {ok, Values}} = memcached_drv:mget(Pool, 0, dict:fetch_keys(KeyDict)),
    lists:foldl(fun({Key, Val, Flag}, Acc) ->
                    case dict:find(Key, KeyDict) of
                        error -> Acc;
                        {ok, K} -> [{K, mcache_util:decode_value({Val, Flag})}|Acc]
                    end
                end, [], Values).

set(Class, Key, Value, Format, Expiry) ->
    {Pool, DefaultExpiry} = mcache_expires:expire(Class),
	{Value1, Flags} = mcache_util:encode_value(Value, Format),
	Expiry1 = mcache_util:encode_expiry(Expiry, DefaultExpiry),
    {mc_async, 0, ok} = memcached_drv:set(Pool, 0, mcache_util:map_key(Class, Key), Value1, Flags, Expiry1),
    ok.

delete(Class, Key, Expiry) ->
    {Pool, _} = mcache_expires:expire(Class),
	Expiry1 = mcache_util:encode_expiry(Expiry, 0),
    {mc_async, 0, ok} = memcached_drv:delete(Pool, 0, mcache_util:map_key(Class, Key), Expiry1).
