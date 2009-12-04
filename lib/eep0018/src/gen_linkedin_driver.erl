-module(gen_linkedin_driver).
-author('echou327@gmail.com').
-vsn('$Revision: 427 $ ').

-behaviour(gen_server).

% API
-export([behaviour_info/1, start/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([control/3]).

-record(state, {module, driver_info}).

-define(PORT_COUNT, 4).
-define(LIB_PATH, "priv/lib").

behaviour_info(callbacks) ->
	[{driver_info,0}];
behaviour_info(_Other) ->
	undefined.

start(Mod) ->
    gen_server:start(?MODULE, [Mod], []).

start_link(Mod) ->
    gen_server:start_link(?MODULE, [Mod], []).

get_lib_path() ->
	?LIB_PATH.

init([CallbackModule]) ->
    DriverInfo = CallbackModule:driver_info(),
	Info = {DriverName, DriverTag, DriverTable} = DriverInfo,
    erl_ddll:load_driver(get_lib_path(), DriverName),
    ets:new(DriverTable, [set, public, named_table]),
    lists:foreach(fun(I) ->
                    Port = open_port({spawn_driver, DriverName}, []),
                    ets:insert(DriverTable, {{DriverTag, I}, Port})
                end, 
                lists:seq(1, ?PORT_COUNT)),
    {ok, #state{module=CallbackModule, driver_info=DriverInfo}}.


%%% --------------------------------------------------------
%%% The call-back functions.
%%% --------------------------------------------------------

handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({'EXIT', Port, Reason}, State) ->
	{DriverName, _, DriverTable} = State#state.driver_info,
    case ets:match(DriverTable, {'$1', Port}) of
        [] ->
            %?DEBUG("Port ~p died: ~p", [Port, Reason]);
            ok;
        [[PortName]|_] ->
            %?DEBUG("Port died because ~p, will add a new one ...", [Reason]),
            NewPort = open_port({spawn_driver, DriverName}, []),
            ets:insert(DriverTable, {PortName, NewPort});
        _Any ->
            %?DEBUG("What is this? : ~p", [_Any]),
            ok
    end,
    {noreply, State};

handle_info(_, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, State) ->
	{_, _, DriverTable} = State#state.driver_info,
    Ports = ets:tab2list(DriverTable),
    lists:foreach(fun(_PortName, Port) -> port_close(Port) end, Ports),
    ok.


get_driver_port(CallbackModule) ->
	{_DriverName, DriverTag, DriverTable} = CallbackModule:driver_info(),
    PortIndex = erlang:system_info(scheduler_id) rem ?PORT_COUNT + 1,
    [{_, Port} | _] = ets:lookup(DriverTable, {DriverTag, PortIndex}),
    Port.

% API 

control(Module, Cmd, Data) ->
    Port = get_driver_port(Module),
    erlang:port_control(Port, Cmd, Data).
