-module(mcache).
-author('echou327@gmail.com').

%-compile([inline, native, {hipe, o3}]).

-export([get_server/2, get/2, mget/2, set/5, set/4, delete/2]).

-define(SEP, ":").
-define(MGET_TIMEOUT, 500).
-define(FMT_RAW, 0).
%-define(FMT_BJSON, 100). 
-define(FMT_NATIVE, 101).
-define(FMT_JSON, 102).
-define(FMT_INT, 103).

get(Class, Key) ->
    {Key1, Server, _DefaultExpiry} = get_server(Class, Key),
    {_, Value} = mcache_client:mc_get(Server, Key1),
	decode_value(Value).

mget(Class, [_|_]=Keys) ->
    {KeyDict, ServerDict} = lists:foldl(
                                fun(K, {KAcc,SAcc}) ->
                                    {K1, Server, _DefaultExpiry} = get_server(Class, K),
                                    RealKey = iolist_to_binary(K1), % must convert to binary because response key is a binary
                                    {orddict:store(RealKey, K, KAcc), dict:append(Server, RealKey, SAcc)}
                                end, {orddict:new(), dict:new()}, Keys),
    dict:fold(fun(Server, Keys1, Acc) ->
                mcache_client:ab_mget(Server, Keys1),
                Acc
              end, nil, ServerDict),
    ValueDict = mget_receive(orddict:size(KeyDict), ?MGET_TIMEOUT, dict:new()),
    %ValueDict = dict:new(),
    Result = orddict:fold(fun(RealKey, Key, Acc) ->
                        case dict:find(RealKey, ValueDict) of
                            error -> Acc;
                            {ok, undefined} -> Acc;
                            {ok, Value} -> [{Key, decode_value(Value)}|Acc]
                        end
                    end, [], KeyDict),
    lists:reverse(Result).

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

my_now() ->
    erlang:now().

mget_receive(0, _Timeout, D) ->
    D;
mget_receive(_N, Timeout, D) when Timeout =< 0 ->
    D;
mget_receive(N, Timeout, D) ->
    Now = my_now(),
    receive
        Any ->
            case Any of
                {Ref, {mget, Items}} when is_reference(Ref) ->
                    Now1 = my_now(),
                    TimeoutLeft = round(Timeout - timer:now_diff(Now1, Now) / 1000),
                    D1 = dict:merge(fun(_K,_V1,V2) -> V2 end, D, Items),
                    mget_receive(N-dict:size(Items), TimeoutLeft, D1);
                {Ref, {Key, Value}}=Msg when is_reference(Ref) ->
                    Now1 = my_now(),
                    TimeoutLeft = round(Timeout - timer:now_diff(Now1, Now) / 1000),
                    mget_receive(N-1, TimeoutLeft, dict:store(Key, Value, D));
                _ ->
                    Now1 = my_now(),
                    TimeoutLeft = round(Timeout - timer:now_diff(Now1, Now) / 1000),
                    mget_receive(N, TimeoutLeft, D)
            end
    after Timeout ->
        D
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
    [cast(Class), ":"|cast(Key)].

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

