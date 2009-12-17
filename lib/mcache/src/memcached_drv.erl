-module(memcached_drv).

-author('echou327@gmail.com').

-behaviour(gen_server).

% API
-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([set_servers/2, set_async/2]).
-export([mget/2, mget/3, set/5, set/6]).

-record(state, {pool, servers}).

-define(PORT_COUNT, 4).
-define(DRV_NAME, memcached_drv).
-define(DRV_TABLE, memcached_drv_table).
-define(DRV_TAG, memcached_drv).

-define(CMD_SET_SERVERS, 0).
-define(CMD_SET, 1).
-define(CMD_MGET, 2).
-define(CMD_SET_ASYNC, 3).

start_link(PoolName, Servers) ->
    gen_server:start_link(?MODULE, [PoolName, Servers], []).

get_lib_path() ->
	"priv/lib".

init([PoolName, Servers]) ->
    case erl_ddll:load_driver(get_lib_path(), ?DRV_NAME) of
        ok -> ok;
        {error, already_loaded} -> ok;
        {error, Error} -> exit(erl_ddll:format_error(Error))
    end,
    catch ets:new(?DRV_TABLE, [set, public, named_table]),
    lists:foreach(fun(I) ->
                    Port = open_port({spawn_driver, ?DRV_NAME}, []), 
                    erlang:port_control(Port, ?CMD_SET_SERVERS, [Servers, 0]),
                    ets:insert(?DRV_TABLE, {{?DRV_TAG, PoolName, I}, Port}),
                    ok
                end, 
                lists:seq(1, ?PORT_COUNT)),
    pg2:create({?MODULE, PoolName}),
    pg2:join({?MODULE, PoolName}, self()),
    io:format("[memcached_drv] Start pool ~s: ~s~n", [PoolName, Servers]),
    {ok, #state{pool=PoolName,servers=Servers}}.


%%% --------------------------------------------------------
%%% The call-back functions.
%%% --------------------------------------------------------

handle_call(_, _, State) ->
    {noreply, State}.

on_off(on) -> 1;
on_off(_) -> 0.

handle_cast({set_async, OnOff}, State) ->
    [ port_control(Port, ?CMD_SET_ASYNC, [on_off(OnOff)]) || Port <- get_all_driver_ports(State#state.pool) ],
    {noreply, State};

handle_cast({set_servers, Servers}, State) ->
    [ port_control(Port, ?CMD_SET_SERVERS, [Servers, 0]) || Port <- get_all_driver_ports(State#state.pool) ],
    {noreply, State#state{servers=Servers}};

handle_cast(_, State) ->
    {noreply, State}.

handle_info({'EXIT', Port, Reason}, #state{pool=Pool, servers=Servers}=State) ->
    case ets:match(?DRV_TABLE, {{?DRV_TAG, Pool, '$1'}, Port}) of
        [] ->
            ok;
        [[PortIndex]|_] ->
            NewPort = open_port({spawn_driver, ?DRV_NAME}, []),
            ets:insert(?DRV_TABLE, { {?DRV_TAG, Pool, PortIndex}, NewPort}),
            if Servers =/= undefined ->
                port_control(NewPort, ?CMD_SET_SERVERS, [Servers, 0]);
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

% API 

control(Pool, Cmd, Data) ->
    Port = get_driver_port(Pool),
    erlang:port_control(Port, Cmd, Data).

send_command(Port, Command, Timeout) ->
    port_command(Port, Command),
    receive
        Data ->
            Data
        after Timeout->
            {error, timeout}
    end.

send_command(Port, Command) ->
    send_command(Port, Command, 100).

%%%%%%%

set_servers(Pool, Servers) ->
    case pg2:get_local_members({?MODULE, Pool}) of
        [_|_]=Pids ->
            [gen_server:cast(Pid, {set_servers, Servers})||Pid<-Pids],
            ok;
        _ ->
            false
    end.

set_async(Pool, OnOff) ->
    case pg2:get_local_members({?MODULE, Pool}) of
        [_|_]=Pids ->
            [gen_server:cast(Pid, {set_async, OnOff})||Pid<-Pids],
            ok;
        _ ->
            false
    end.

mget(Pool, Keys) -> mget(Pool, 0, Keys).

mget(Pool, Seq, [_|_]=Keys) ->
    Port = get_driver_port(Pool),
    NumKeys = length(Keys),
    Data = lists:foldl(fun(K,A) -> 
                        KLen = iolist_size(K),
                        [K, <<KLen:32>>|A]
                    end, [<<Seq:32, NumKeys:32>>], Keys),
    Data1 = lists:reverse(Data),
    send_command(Port, [?CMD_MGET|Data1]).

set(Pool, Key, Value, Flags, Expires) -> set(Pool, 0, Key, Value, Flags, Expires).

set(Pool, Seq, Key, Value, Flags, Expires) ->
    Port = get_driver_port(Pool),
    KLen = iolist_size(Key),
    VLen = iolist_size(Value),
    send_command(Port, [?CMD_SET, <<Seq:32, KLen:32>>, Key, <<VLen:32>>, Value, <<Flags:32, Expires:32>>]).
    

