-module(memcached_drv).

-author('echou327@gmail.com').

-behaviour(gen_server).

% API
-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-export([set_servers/2]).
-export([get/3, mget/3, set/6, delete/4]).
-export([ab_get/3, ab_mget/3, ab_set/6, ab_delete/4]).

-record(state, {pool, servers}).

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

-define(RECV_TIMEOUT, 1000).

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

%%%%%%%

set_servers(Pool, Servers) ->
    case pg2:get_local_members({?MODULE, Pool}) of
        [_|_]=Pids ->
            [gen_server:cast(Pid, {set_servers, Servers})||Pid<-Pids],
            ok;
        _ ->
            false
    end.


ab_get(Pool, Seq, Key) ->
    Port = get_driver_port(Pool),
    K = to_binary(Key),
    KLen = byte_size(K),
    erlang:port_command(Port, [<<?CMD_GET, Seq:32, KLen:32>>, K]).
    
get(Pool, Seq, Key) ->
    ab_get(Pool, Seq, Key),
    do_receive(?RECV_TIMEOUT).

ab_mget(Pool, Seq, [_|_]=Keys) ->
    Port = get_driver_port(Pool),
    NumKeys = length(Keys),
    Data = lists:foldl(fun(K,A) -> 
                        K1 = to_binary(K),
                        KLen = byte_size(K1),
                        [K1, <<KLen:32>>|A]
                    end, [<<?CMD_MGET, Seq:32, NumKeys:32>>], Keys),
    erlang:port_command(Port, lists:reverse(Data)).

mget(Pool, Seq, Keys) ->
    ab_mget(Pool, Seq, Keys),
    do_receive(?RECV_TIMEOUT).

ab_set(Pool, Seq, Key, Value, Flags, Expires) ->
    Port = get_driver_port(Pool),
    K = to_binary(Key),
    V = to_binary(Value),
    KLen = byte_size(K),
    VLen = byte_size(V),
    erlang:port_command(Port, [<<?CMD_SET, Seq:32, KLen:32, VLen:32, Flags:32, Expires:32>>, K, V]).

set(Pool, Seq, Key, Value, Flags, Expires) ->
    ab_set(Pool, Seq, Key, Value, Flags, Expires),
    do_receive(?RECV_TIMEOUT).
    
ab_delete(Pool, Seq, Key, Expires) ->
    Port = get_driver_port(Pool),
    K = to_binary(Key),
    KLen = byte_size(K),
    erlang:port_command(Port, [<<?CMD_DELETE, Seq:32, Expires:32, KLen:32>>, K]).
    
delete(Pool, Seq, Key, Expires) ->
    ab_delete(Pool, Seq, Key, Expires),
    do_receive(?RECV_TIMEOUT).

