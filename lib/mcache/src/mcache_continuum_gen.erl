-module(mcache_continuum_gen).

-author('echou327@gmail.com').

-export([gen/2]).

-define(HAVE_CASE_WHEN, 1).
%-define(HAVE_FUNC_MATCH, 1).
%-define(HAVE_BINARY_SEARCH, 1).
%-define(HAVE_GB_TREES, 1).

% API exports
-define(UINT32_LITTLE, 32/unsigned-little-integer).
-define(POINTS_PER_SERVER, 160).
-define(DEFAULT_PORT, 11211).

-define(HEADER,
"-module(mcache_continuum).
-author('echou327@gmail.com').
-compile(inline).
-export([find/2]).

% auto-generated code. DO NOT EDIT.

").


gen(Pools, Mode) ->
    Continuums = lists:map(
                    fun({Name, Servers}) ->
                        {S, C} = make(Servers),
                        {Name, S, C}
                    end,
                    Pools),
    Code = codegen(Mode, Continuums),
    {Continuums, compile(Code)}.

% make continuum

my_floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;
        Pos when Pos > 0 -> T;
        _ -> T
    end.

my_ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

points_per_server(Weight, TotalWeight, NumberOfServers) -> 
    Pct = Weight / TotalWeight,
    my_floor(Pct * ?POINTS_PER_SERVER/ 4 * NumberOfServers + 0.0000000001) * 4.

calc_total_weight([{_IP,_Port,Weight}|R], W) ->
    calc_total_weight(R, Weight+W);
calc_total_weight([], W) ->
    W.

standardize_server_list({IP,Ports}) when is_list(Ports) ->
	[standardize_server_list({IP, Port})||Port <- Ports];
standardize_server_list({IP,Port,Weight}) ->
    [{IP,Port,Weight}];
standardize_server_list({IP,{Port,Weight}}) ->
    [{IP,Port,Weight}];
standardize_server_list({IP,Port}) ->
    [{IP,Port,100}].

server_key({A,B,C,D}, ?DEFAULT_PORT, Index) ->
	io_lib:format("~p.~p.~p.~p-~p", [A,B,C,D,Index-1]);
server_key({A,B,C,D}, Port, Index) ->
	io_lib:format("~p.~p.~p.~p:~p-~p", [A,B,C,D,Port,Index-1]).

ketama_server_hash(Key) ->
    <<Int1 : ?UINT32_LITTLE,  Int2 : ?UINT32_LITTLE, Int3 : ?UINT32_LITTLE, Int4 : ?UINT32_LITTLE>> = erlang:md5(Key),
    [Int1, Int2, Int3, Int4].

% API
make(Servers) ->
    Servers1 = lists:flatmap(fun standardize_server_list/1, Servers),
    TotalWeight = calc_total_weight(Servers1, 0),
	make(Servers1, TotalWeight).

make([], _TotalWeight) ->
	throw(no_servers);
make(Servers, TotalWeight) ->
    PointsPerHash = 4,
	NumberOfServers = length(Servers),
    {_, Continuum, ServersRev} = lists:foldl(
						fun({IP,Port,Weight}, {ServerIndex, ContinuumAcc, ServerAcc}) ->
							Points = points_per_server(Weight, TotalWeight, NumberOfServers),
							N = my_ceiling(Points/PointsPerHash),
							R = lists:flatmap(
									fun(Index) ->
										Key = server_key(IP,Port,Index),
										[{Int,ServerIndex}||Int <- ketama_server_hash(Key)]
									end,
									lists:seq(1,N)),
							{ServerIndex+1, R ++ ContinuumAcc, [{IP,Port}|ServerAcc] }
						end, 
                        {1, [], []},
						Servers),
    {lists:reverse(ServersRev), lists:keysort(1, Continuum)}.


func_match_clause({-1, Index}) ->
    io_lib:format("find_1(_H) ->~n    ~p.~n", [Index]);
func_match_clause({K, Index}) ->
    io_lib:format("find_1(H) when H<~p ->~n    ~p;~n", [K, Index]).

% API

codegen(case_when, Continuums) ->

    ServersFunc = [ lists:map(
                        fun({PoolName, Servers, _Continuum}) ->
                            io_lib:format("servers(~p) -> ~p;~n", [PoolName, list_to_tuple(Servers)])
                        end,
                        Continuums),
                    "servers(_) -> erlang:error(not_implemented).\n\n"],

    FindFunc = 
"find(PoolName, Hash) ->
    Index = find_1(PoolName, Hash),
    element(Index, servers(PoolName)).

",
    
    Find1Func = lists:map(
                    fun({PoolName, _Servers, [{_,SmallestIndex}|_]=Continuum}) ->
                        [
io_lib:format("find_1(~p, H) ->~n",[PoolName]),    
io_lib:format("    case H of~n",[]),
[
io_lib:format("        H when H<~p -> ~p;~n",[K,I]) || {K,I} <-Continuum
],
io_lib:format("        _ -> ~p~n",[SmallestIndex]),
io_lib:format("    end;~n",[])
                        ]
                    end,
                    Continuums),
                    
    lists:flatten([?HEADER, ServersFunc, FindFunc, Find1Func, "find_1(_,_) -> erlang:error(not_implemented).\n"]);

codegen(func_match, {Servers, Continuum}) ->

    [{_,SmallestIndex}|_] = Continuum,
    Clauses = [func_match_clause(X) || X<-Continuum],
    TermClause = func_match_clause({-1, SmallestIndex}),
    
    Func = io_lib:format(
"-define(SERVERS, ~p).

find(H) ->
    Index = find_1(H),
    element(Index, ?SERVERS).

", [list_to_tuple(Servers)]),

    lists:flatten([?HEADER, Func, Clauses, TermClause]);

codegen(binary_search, {Servers, Continuum}) ->
	ContinuumSize = length(Continuum),
	ServersTuple = list_to_tuple(Servers),
	ContinuumTuple = list_to_tuple(Continuum),

	Func = io_lib:format(
"-define(CONTINUUM_SIZE, ~p).
continuum() ->
    ~p.
servers() ->
    ~p.

find(Hash) ->
	Index = find_server_index(Hash, 0, ?CONTINUUM_SIZE),
    Item = element(Index+1, continuum()),
    ServerIndex = element(2, Item),
    element(ServerIndex, servers()).

% binary search
find_server_index(Hash, LowP, HighP) ->
    MidP = round((LowP + HighP)/2),
    MaxP = ?CONTINUUM_SIZE,
    if MidP =:= MaxP; LowP > HighP ->
        0;
    true ->
        MidVal = continuum_val(MidP),
        MidVal1 = continuum_val(MidP-1),
        if Hash =< MidVal1 ->
            find_server_index(Hash, LowP, MidP-1);
        Hash > MidVal ->
            find_server_index(Hash, MidP+1, HighP);
        true ->
            MidP
        end
    end.

continuum_val(Index) when Index<0 ->
	0;
continuum_val(Index) ->
    Item = element(Index+1, continuum()),
    element(1, Item).

", [ContinuumSize, ContinuumTuple, ServersTuple]),
	
    lists:flatten([?HEADER, Func]);

codegen(gb_trees, {Servers, Continuum}) ->
    {_,C} = gb_trees:from_orddict(Continuum),

	Func = io_lib:format(
"-define(CONTINUUM, ~p).
-define(SERVERS, ~p).
	
find(Hash) ->
	ServerIndex = find_1(Hash, ?CONTINUUM),
    element(ServerIndex, ?SERVERS).

find_1(_H, nil) -> 1;
find_1(H,{K,V,_,L}) when H>K ->
    case L of
        nil -> V;
        {K1,_,_,_} when H<K1 -> V;
        _ -> find_1(H,L)
    end;
find_1(H,{K,_,S,_}) when H<K ->
    find_1(H,S);
find_1(H,{K,V,_,_}) when H=:=K ->
    V.
",
[C, list_to_tuple(Servers)]),
	
    lists:flatten([?HEADER, Func]);

codegen(_, _) ->
    erlang:error(not_implemented).


compile(Code) ->
	{M, B} = dynamic_compile:from_string(Code),
    code:load_binary(M, "", B),
    Code.

