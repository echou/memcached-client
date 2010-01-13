-module(mcache_util).
%%%=========================================================================
%%%  Memcached utils helper
%%%=========================================================================
-author('thijsterlouw@gmail.com').

-compile([inline, native, {hipe, o3}]).

-export([now/0, hash/2, floor/1, ceiling/1, get_app_env/2, make_atom_name/2, make_atom_name/3, reload_config/1, sup_child_spec/3, sup_child_spec/2]).
-export([gb_trees_find/4]).
-export([map_key/2, decode_value/1, encode_value/2, encode_expiry/2]).

now() ->
	{MegaS, S, _MicroS} = erlang:now(),
	MegaS*1000000 + S.

hash(Key, crc) ->
    erlang:crc32(Key);
hash(Key, erlang) ->
    erlang:phash2(Key);
hash(Key, md5) ->
    <<Int:32/unsigned-little-integer,  _:12/binary>> = erlang:md5(Key),
    Int.


floor(X) ->
	T = erlang:trunc(X),
	case (X - T) of
		Neg when Neg < 0 -> T - 1;
		Pos when Pos > 0 -> T;
		_ -> T
	end.

ceiling(X) ->
	T = erlang:trunc(X),
	case (X - T) of
		Neg when Neg < 0 -> T;
		Pos when Pos > 0 -> T + 1;
		_ -> T
	end.

get_app_env(Opt, Default) ->
	case application:get_env(Opt) of
		{ok, Val} -> Val;
		_ ->
			case init:get_argument(Opt) of
				{ok, [[Val | _]]} 	-> Val;
				error       		-> Default
			end
	end.

make_atom_name(First, Second) ->
	list_to_atom(lists:flatten(io_lib:format("~p_~p", [First, Second]))).	
	
make_atom_name(First, Second, Third) ->
	list_to_atom(lists:flatten(io_lib:format("~p_~p_~p", [First, Second, Third]))).	


format_atom(Format, Args) ->
    list_to_atom(lists:flatten(io_lib:format(Format, Args))).
    % }}}

% 是否在本node中启动该进程, 如果不设置，缺省为启动
module_enabled(Module) ->
    module_enabled(Module, 1).

module_enabled(Module, DefaultCount) ->
    Key = format_atom("enable_~p", [Module]),
    get_app_env(Key, DefaultCount).

sup_child_spec(Module, Fun, Count) ->
    N = module_enabled(Module, Count),
    lists:map(fun(I) ->
        % Id = format_atom("~p_~p", [Module, I]),
        Fun(Module, {Module, I})
    end, lists:seq(0, N-1)).

sup_child_spec(Module, Fun) ->
    sup_child_spec(Module, Fun, 1).


%supply a list of application names to restart (as atoms). 
%function will only reload config for apps already started
reload_config(AppNames) ->
	case init:get_argument(config) of
    		{ok, [ Files ]} ->
	         	ConfFiles = [begin
		                          S = filename:basename(F,".config"),
		                          filename:join(filename:dirname(F),  S ++ ".config")
		                      end || F <- Files],
	                      
	         	% Move sys.config to the head of the list
	         	Config = lists:sort(fun("sys.config", _) -> true;
	                                		(_, _) -> false 
	                                	end, 
	                                	ConfFiles),

	         	OldEnv = application_controller:prep_config_change(),

			WhichApplicationNamesSet =  sets:from_list([ A || {A,_,_} <- application:which_applications()]),
			AppNamesSet = sets:from_list(AppNames),
			AllowedApps = sets:intersection(WhichApplicationNamesSet, AppNamesSet),
			
	         	Apps = [{application, A, make_appl(A)} || A <- sets:to_list(AllowedApps)],
	         	application_controller:change_application_data(Apps, Config),
	         	application_controller:config_change(OldEnv);
		_ ->
	       	{ok, []}
     end.

make_appl(App) when is_atom(App) ->
	AppList  = element(2, application:get_all_key(App)),
	FullName = code:where_is_file(atom_to_list(App) ++ ".app"),
	case file:consult(FullName) of
		{ok, [{application, _, Opts}]} ->
			 Env = proplists:get_value(env, Opts, []),
			 lists:keyreplace(env, 1, AppList, {env, Env});
		{error, _Reason} ->
	 		lists:keyreplace(env, 1, AppList, {env, []})
	end.



gb_trees_find(H, nil, {LH, _} = L, L) when H >= LH ->
    -1;
gb_trees_find(H, nil, R, {RH,_} = R) when H < RH ->
    -2;
gb_trees_find(H, nil, L, {RH, I}=R) ->
    I;
gb_trees_find(H, {K,V,_,_}, _Left, _Right) when H=:=K ->
    V;
gb_trees_find(H, {K,V,S,_}, Left, _Right) when H<K->
    gb_trees_find(H, S, Left, {K,V});
gb_trees_find(H, {K,V,_,L}, _Left, Right) when H>K ->
    gb_trees_find(H, L, {K,V}, Right).


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

-define(FMT_RAW, 0).
-define(FMT_NATIVE, 101).
-define(FMT_JSON, 102).
-define(FMT_INT, 103).


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
	Int;
decode_value(Any) ->
    erlang:error({unknown_value, Any}).

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
