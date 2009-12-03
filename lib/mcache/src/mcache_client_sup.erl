-module(mcache_client_sup).
-author('echou327@gmail.com').

-behaviour(supervisor).
-export([init/1]).

-export([start_link/0, start_child/3, restart_child/2, terminate_child/2]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    error_logger:info_msg("[MCACHE_CLIENT_SUP] Started.~n"),

    {ok, {{one_for_one, 10, 10}, []}}.

start_child(Name, {Host, Port}, Num) ->
    lists:foreach(
        fun(I) ->
            Id = {mcache_client, {Name, Host, Port}, I},
			Spec = {Id,{mcache_client,start_link,[{Host,Port}]},permanent,2000,worker,[mcache_client]},
            case supervisor:start_child(?MODULE, Spec) of
				{ok, _} -> ok;
				{ok, _, _} -> ok;
				{error, {already_started,_}} ->
					supervisor:terminate_child(?MODULE, Id),
					supervisor:delete_child(?MODULE, Id),
					supervisor:start_child(?MODULE, Spec);
				{error, already_present} ->
					supervisor:delete_child(?MODULE, Id),
					supervisor:start_child(?MODULE, Spec);
				{error, _Error} ->
					erlang:error(_Error)
			end
        end,
        lists:seq(1, Num)).

terminate_child(Name, {Host, Port}) ->
    F = fun({{mcache_client, {Name1, Host1, Port1}, _Index} = Id, _Child, _Type, _Modules}) when Name=:=Name1,Host=:=Host1,Port=:=Port1-> 
				supervisor:terminate_child(?MODULE, Id);
           (_) -> 
				ignore
        end,
    lists:foreach(F, supervisor:which_children(?MODULE)).

restart_child(Name, {Host, Port}) ->
    F = fun({{mcache_client, {Name1, Host1, Port1}, _Index} = Id, _Child, _Type, _Modules}) when Name=:=Name1,Host=:=Host1,Port=:=Port1-> 
				supervisor:restart_child(?MODULE, Id);
           (_) -> 
				ignore
        end,
    lists:foreach(F, supervisor:which_children(?MODULE)).
