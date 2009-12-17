%
% wraps memcached_drv
%

-module(mcache2).
-author('echou327@gmail.com').

-export([mget/2, set/5, set/4]).

-define(SEP, ":").
-define(MGET_TIMEOUT, 500).
-define(FMT_RAW, 0).
-define(FMT_NATIVE, 101).
-define(FMT_JSON, 102).
-define(FMT_INT, 103).

mget(Class, [_|_]=Keys) ->
    {Pool, _Expiry} = mcache_expires:expire(Class),
    RealKeys = lists:map(fun(K) ->
                            K1 = map_key(Class, K),
                            erlang:put({'$mcache2_mget$', K1}, K),
                            K1
                        end, Keys),
    try
        {mc_async, 0, {ok, Values}} = memcached_drv:mget(Pool, 0, RealKeys),
        lists:foldl(fun({Key, Val, Flag}, Acc) ->
                        case erlang:get({'$mcache2_mget$', Key}) of
                            undefined ->
                                Acc;
                            K ->
                                [{K, decode_value({Val, Flag})}|Acc]
                        end
                    end, [], Values)
    after 
        lists:foreach(fun({{'$mcache2_mget$',_}=K,V}) ->
                        erlang:erase(K)
                    end, erlang:get())
    end.

set(Class, Key, Value, Format, Expiry) ->
    {Pool, DefaultExpiry} = mcache_expires:expire(Class),
	{Value1, Flags} = encode_value(Value, Format),
	Expiry1 = encode_expiry(Expiry, DefaultExpiry),
    {mc_async, 0, {ok}} = memcached_drv:set(Pool, 0, map_key(Class, Key), Value1, Flags, Expiry1),
    ok.

set(Class, Key, Value, Format) ->
    set(Class, Key, Value, Format, default).


% internal functions
my_now() ->
    erlang:now().

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

