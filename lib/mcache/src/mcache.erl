-module(mcache).
-author('echou327@gmail.com').

%-compile([inline]). 
%-compile([native, {hipe, o3}]).

-export([get_server/2, get/2, mget/2, set/5, set/4, delete/2]).

-define(SEP, ":").
-define(MGET_TIMEOUT, 500).
-define(FMT_RAW, 0).
-define(FMT_NATIVE, 101).
-define(FMT_JSON, 102).
-define(FMT_INT, 103).


get(Class, Key) ->
    {Key1, Server, _DefaultExpiry} = get_server(Class, Key),
    {_, Value} = mcache_client:mc_get(Server, Key1),
	decode_value(Value).

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
    flat_foldl(
        fun({RealKey, Val}, LAcc) ->
            case dict:find(RealKey, KeyDict) of
                {ok, Key} ->
                    [{Key, decode_value(Val)}|LAcc];
                _ ->
                    LAcc
            end
        end, [], Results).

set(Class, Key, Value, Format, Expiry) ->
	{Key1, Server, DefaultExpiry} = get_server(Class, Key),
	{Value1, Flags} = encode_value(Value, Format),
	Expiry1 = encode_expiry(Expiry, DefaultExpiry),
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

cast([H]) when H>255;is_atom(H) ->
    cast(H);
cast([H|L]) when H>255;is_atom(H) ->
    [cast(H),":"|cast(L)];
cast(V) when is_tuple(V) ->
    cast(tuple_to_list(V));
cast(V) when is_list(V); is_binary(V) ->
    V;
cast(V) when is_atom(V) ->
    atom_to_list(V);
cast(V) when is_integer(V) ->
    integer_to_list(V).

map_key(Class, Key) ->
    iolist_to_binary([cast(Class), ":", cast(Key)]).

get_server(Class, Key) ->
    Key1 = map_key(Class, Key),
    {Pool, Expiry} = mcache_expires:expire(Class),
    Server = mcache_continuum:find(Pool, mcache_util:hash(Key1, md5)),
    {Key1, Server, Expiry}.


encode_value(Value, raw) ->
	{Value, ?FMT_RAW};
encode_value(Value, native) ->
	{term_to_binary(Value), ?FMT_NATIVE};
encode_value(Value, json) ->
	{eep0018:encode(Value), ?FMT_JSON};
encode_value(Value, int) ->
	{<<Value:32>>, ?FMT_INT}.

decode_value(undefined) ->
	undefined;
decode_value(not_found) ->
    undefined;
decode_value({Data, ?FMT_RAW}) ->
	Data;
decode_value({Data, ?FMT_NATIVE}) ->
	binary_to_term(Data);
decode_value({Data, ?FMT_JSON}) ->
	eep0018:decode(Data);
decode_value({<<Int:32>>, ?FMT_INT}) ->
	Int.

encode_expiry(default, DefaultExpiry) ->
    encode_expiry1(DefaultExpiry);
encode_expiry(Expiry, _DefaultExpiry) ->
    encode_expiry1(Expiry).

encode_expiry1(infinity) ->
    0;
encode_expiry1({X, seconds}) ->
    X;
encode_expiry1({X, minutes}) ->
    X*60;
encode_expiry1({X, hours}) ->
    X*3600;
encode_expiry1({X, days}) when X<30->
    X*86400;
encode_expiry1(X) when is_integer(X) ->
    X.

