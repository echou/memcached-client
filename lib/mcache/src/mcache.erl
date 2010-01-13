-module(mcache).
-author('echou327@gmail.com').

%-compile([inline]). 
%-compile([native, {hipe, o3}]).

-export([get_server/2, get/2, mget/2, set/5, set/4, delete/2]).

-define(SEP, ":").
-define(MGET_TIMEOUT, 500).

get(Class, Key) ->
    {Key1, Server, _DefaultExpiry} = get_server(Class, Key),
    {_, Value} = mcache_client:mc_get(Server, Key1),
    %io:format("get ~p~n", [Value]),
	mcache_util:decode_value(Value).

mget(Class, [_|_]=Keys) ->
    erlang:yield(),
    {KeyDict, ServerDict} = lists:foldl(
                                fun(K, {KAcc,SAcc}) ->
                                    {K1, Server, _DefaultExpiry} = get_server(Class, K),
                                    {dict:store(K1, K, KAcc), dict:append(Server, K1, SAcc)}
                                end, {dict:new(), dict:new()}, Keys),
    Ref = erlang:make_ref(),
    dict:map(fun(Server, Ks) -> 
                mcache_client:ab_mget(Server, Ref, Ks) 
            end, ServerDict),
    Results = mget_receive(dict:size(KeyDict), Ref, ?MGET_TIMEOUT*1000, []),
    %io:format("~p~n", [Results]),
    flat_foldl(
        fun({RealKey, Val}, LAcc) ->
            case dict:find(RealKey, KeyDict) of
                {ok, Key} ->
                    [{Key, mcache_util:decode_value(Val)}|LAcc];
                _ ->
                    LAcc
            end
        end, [], Results).

mget2(Class, [_|_]=Keys) ->
    ServerDict = lists:foldl(
                    fun(K, SAcc) ->
                        {K1, Server, _DefaultExpiry} = get_server(Class, K),
                        dict:append(Server, K1, SAcc)
                    end, dict:new(), Keys),
    Ref = erlang:make_ref(),
    dict:map(fun(Server, Ks) -> 
                mcache_client:ab_mget(Server, Ref, Ks) 
            end, ServerDict),
    Results = mget_receive(length(Keys), Ref, ?MGET_TIMEOUT*1000, []).

set(Class, Key, Value, Format, Expiry) ->
	{Key1, Server, DefaultExpiry} = get_server(Class, Key),
	{Value1, Flags} = mcache_util:encode_value(Value, Format),
	Expiry1 = mcache_util:encode_expiry(Expiry, DefaultExpiry),
	mcache_client:ab_set(Server, Key1, Value1, Flags, Expiry1),
	ok.

set(Class, Key, Value, Format) ->
    set(Class, Key, Value, Format, default).

delete(Class, Key) ->
	{Key1, Server} = get_server(Class, Key),
	mcache_client:ab_delete(Server, Key1),
	ok.

% internal functions

flat_foldl(_Fun, Acc, []) ->
    Acc;
flat_foldl(Fun, Acc, [H|T]) ->
    Acc1 = case H of
            [] -> Acc;
            [_|_] -> flat_foldl(Fun, Acc, H);
            _ -> Fun(H, Acc)
        end,
    flat_foldl(Fun, Acc1, T).

my_now() ->
    erlang:now().

wrap_items(nil, L) ->
    L;
wrap_items(Items, L) ->
    [Items|L].

mget_receive(N, _Ref, Timeout, L) when N =< 0; Timeout =< 0 ->
    L;
mget_receive(N, Ref, Timeout, L) ->
    Now = my_now(),
    TimeoutMillis = Timeout div 1000,
    receive
        {Ref, {mget, NumKeys, Items}} -> 
            Now1 = my_now(),
            T1 = Timeout - timer:now_diff(Now1, Now),
            mget_receive(N-NumKeys, Ref, T1, wrap_items(Items, L))
    after TimeoutMillis ->
        L
    end.

get_server(Class, Key) ->
    Key1 = mcache_util:map_key(Class, Key),
    {Pool, Expiry} = mcache_expires:expire(Class),
    Server = mcache_continuum:find(Pool, mcache_util:hash(Key1, md5)),
    {Key1, Server, Expiry}.
