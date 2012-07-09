-module(memcached_drv).

-author('echou327@gmail.com').

-behaviour(gen_server).

% API
-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-export([get/3, get/4, mget/3, mget/4, set/7, delete/3]).
-export([ab_get/3, ab_mget/3]).

-record(state, {pool, options}).

-define(PORT_COUNT, 4).
-define(DRV_NAME, memcached_drv).
-define(DRV_TABLE, memcached_drv_table).
-define(DRV_TAG, memcached_drv).

-define(CMD_SET_SERVERS, 0).
-define(CMD_GET, 1).
-define(CMD_MGET, 2).
-define(CMD_SET, 3).
-define(CMD_DELETE, 4).
-define(CMD_MGET2, 5).
-define(CMD_SET_NOREPLY,13).
-define(CMD_DELETE_NOREPLY,14).

-define(RECV_TIMEOUT, 1000).

start_link(PoolName, Options) ->
    gen_server:start_link(?MODULE, [PoolName, Options], []).

get_lib_path() ->
    app_util:get_lib_path().

init([PoolName, Options]=_Args) ->
    LibDir = case code:lib_dir(mcache) of
                {error, _} -> "priv/lib";
                Any -> Any
            end,

    error_logger:info_msg("init memcached_drv: ~s/~s.so~n", [LibDir, ?DRV_NAME]),


    case erl_ddll:load_driver(LibDir, ?DRV_NAME) of
        ok -> ok;
        {error, already_loaded} -> ok;
        {error, Error} -> exit(erl_ddll:format_error(Error))
    end,
    IsBinary = proplists:get_value(binary, Options, true),
    Servers = proplists:get_value(servers, Options, []),
    %io:format("~p:init(~p) ~p, ~s~n", [?MODULE, _Args, bool(IsBinary), Servers]),
    catch ets:new(?DRV_TABLE, [set, public, named_table]),
    lists:foreach(fun(I) ->
                    Port = open_port({spawn_driver, ?DRV_NAME}, []), 
                    do_set_servers(Port, IsBinary, Servers),
                    ets:insert(?DRV_TABLE, {{?DRV_TAG, PoolName, I}, Port}),
                    ok
                end, 
                lists:seq(1, ?PORT_COUNT)),
    pg2:create({?MODULE, PoolName}),
    pg2:join({?MODULE, PoolName}, self()),
    io:format("[~s] Start pool ~s: ~s~n", [?MODULE, PoolName, Servers]),
    {ok, #state{pool=PoolName,options=Options}}.


handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Port, Reason}, #state{pool=Pool, options=Options}=State) ->
    case ets:match(?DRV_TABLE, {{?DRV_TAG, Pool, '$1'}, Port}) of
        [] ->
            ok;
        [[PortIndex]|_] ->
            NewPort = open_port({spawn_driver, ?DRV_NAME}, []),
            ets:insert(?DRV_TABLE, { {?DRV_TAG, Pool, PortIndex}, NewPort}),
            IsBinary = proplists:get_value(binary, Options, true),
            Servers = proplists:get_value(servers, Options, []),
            if Servers =/= undefined ->
                do_set_servers(NewPort, IsBinary, Servers);
            true ->
                ok
            end;
        _Any ->
            ok
    end,
    {noreply, State};

handle_info(_Msg, State) ->
    error_logger:info_msg("handle_info(~p)~n", [_Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, #state{pool=Pool}=State) ->
    case ets:match(?DRV_TABLE, {{?DRV_TAG, Pool, '$1'}, '$2'}) of
        [_|_]=Ports ->
            lists:foreach(fun([Index, Port]) -> 
                            port_close(Port),
                            ets:delete(?DRV_TABLE, {?DRV_TAG, Pool, Index})
                    end, Ports),
            ok;
        _ -> ok
    end.

get_all_driver_ports(Pool) ->
    case ets:match(?DRV_TABLE, {{?DRV_TAG, Pool, '_'}, '$1'}) of
        [_|_]=Ports ->
            [Port||[Port]<-Ports];
        _ -> 
            []
    end.

get_driver_port(Pool) ->
    %{_,_,X} = erlang:now(),
	%PortIndex = (X  rem ?PORT_COUNT) + 1,
	PortIndex = (erlang:system_info(scheduler_id) rem ?PORT_COUNT)+1,
    [{_, Port} | _] = ets:lookup(?DRV_TABLE, {?DRV_TAG, Pool, PortIndex}),
    Port.

do_receive(Timeout) ->
    receive 
        {mc_async, _, _} = Msg -> Msg
    after Timeout ->
        {error, timeout}
    end.

            
control(Pool, Cmd, Data) ->
    Port = get_driver_port(Pool),
    erlang:port_control(Port, Cmd, Data).

to_binary(B) when is_binary(B) ->
    B;
to_binary(L) when is_list(L) ->
    iolist_to_binary(L);
to_binary(A) when is_atom(A) ->
    list_to_binary(atom_to_list(A));
to_binary(N) when is_integer(N) ->
    list_to_binary(integer_to_list(N));
to_binary({Class,Key}) ->
    C = to_binary(Class),
    K = to_binary(Key),
    <<C/binary, ":", K/binary>>.

bool(true) -> 1;
bool(false) -> 0;
bool(on) -> 1;
bool(off) -> 0;
bool(yes) -> 1;
bool(no) -> 0;
bool(X) when is_integer(X),X=/=0 -> 1;
bool(_) -> 0.

do_set_servers(Port, IsBinary, Servers) ->
    erlang:port_control(Port, ?CMD_SET_SERVERS, [bool(IsBinary), Servers, 0]).

%%%%%%%

ab_get(Pool, Seq, Key) ->
    Port = get_driver_port(Pool),
    K = to_binary(Key),
    KLen = byte_size(K),
    erlang:port_command(Port, [<<?CMD_GET, Seq:32, KLen:32>>, K]).
    
get(Pool, Seq, Key, Timeout) ->
    ab_get(Pool, Seq, Key),
    do_receive(Timeout).

get(Pool, Seq, Key) ->
    get(Pool, Seq, Key, ?RECV_TIMEOUT).

ab_mget(Pool, Seq, [_|_]=Keys) ->
    Port = get_driver_port(Pool),
    NumKeys = length(Keys),
    Data = lists:foldl(fun(K,A) -> 
                        K1 = to_binary(K),
                        KLen = byte_size(K1),
                        <<A/binary, KLen:32, K1/binary>>
                    end, <<?CMD_MGET, Seq:32, NumKeys:32>>, Keys),
    erlang:port_command(Port, Data).

mget(Pool, Seq, Keys, Timeout) ->
    ab_mget(Pool, Seq, Keys),
    do_receive(Timeout).

mget(Pool, Seq, Keys) ->
    mget(Pool, Seq, Keys, ?RECV_TIMEOUT).

set(Pool, Seq, Op, Key, Value, Flags, Expires) ->
    Port = get_driver_port(Pool),
    K = to_binary(Key),
    V = to_binary(Value),
    KLen = byte_size(K),
    VLen = byte_size(V),
    Op1 = case Op of
            set -> $s;
            add -> $a;
            replace -> $r;
            _ -> $s
        end,
    erlang:port_command(Port, [<<?CMD_SET_NOREPLY, Seq:32, Op1:8, KLen:32, VLen:32, Flags:32, Expires:32>>, K, V]).

delete(Pool, Seq, Key) ->
    Port = get_driver_port(Pool),
    K = to_binary(Key),
    KLen = byte_size(K),
    erlang:port_command(Port, [<<?CMD_DELETE_NOREPLY, Seq:32, KLen:32>>, K]).

