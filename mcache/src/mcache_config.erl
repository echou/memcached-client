-module(mcache_config).
-author('thijsterlouw@gmail.com').
-author('echou327@gmail.com').

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

-export([update_expires/1,get_expires/0]).

-define(DEFAULT_EXPIRE_SECONDS, 300).		%5 minutes
-define(DEFAULT_POOL, generic).

-record(state, {expires, initial_expires}).

%%%=========================================================================
%%%  Start the config
%%%=========================================================================

start_link(Opts) ->
    	gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

init({Pools, Expires}=Opts) ->
	io:format("[MCACHE_CONFIG] init~n", []),
	process_flag(trap_exit, true),
	
	parse_expires(Expires),
	parse_pools(Pools),
	{ok, #state{expires=Expires, initial_expires=Expires}}.


handle_call({update_expires, Info}, _From, #state{expires=OldExpires, initial_expires=InitialExpires}=State) ->
	NewExpires = case Info of
					restore ->		
						% »Ö¸´³õÊ¼ÅäÖÃ
						InitialExpires;
					{assign, Expires} -> 
						% ÉèÖÃÐÂÒ»Ì×ÅäÖÃ
						Expires;
					{delete, Class} ->
						% É¾³ýÄ³¸öClassµÄÅäÖÃ
						proplists:delete(Class, OldExpires);
					{set, Class, {PoolName, Expiry}} -> 
						% ÉèÖÃÄ³¸öClassÅäÖÃ
						Expires1 = proplists:delete(Class, OldExpires),
						[{Class, {PoolName, Expiry}}|Expires1];
					_ ->
						not_modified
				end,
	if NewExpires =:= not_modified ->
		{reply, ok, State};
	true ->
		case (catch parse_expires(NewExpires)) of
			{'EXIT', Reason} ->
				{reply, {error, Reason}, State};
			_ ->
				{reply, ok, #state{expires=NewExpires}}
		end
	end;

handle_call(get_expires, _From, #state{expires=Expires}=State) ->
    {reply, {ok, Expires}, State};

handle_call(_Request, _From, State) ->
	{noreply, State}.

%handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Request, State) -> 
	{noreply, State}.

handle_info(_Info, State) -> 
	{noreply, State}.

terminate(Reason, _State) ->
	io:format("[MCACHE_CONFIG] terminate() reason: ~p~n", [Reason]).

code_change(_OldVsn, _Extra, State) ->
    io:format("blabla~n",[]),
	State.


% API
update_expires(UpdateInfo) ->
	gen_server:call(?MODULE, {update_expires, UpdateInfo}).

get_expires() ->
	gen_server:call(?MODULE, get_expires).



% internal functions

parse_expires(Expires) ->
	io:format("[MCACHE_CONFIG] Generating mcache_expires module ...~n", []),

	Code =
[
"-module(mcache_expires).\n-author('echou327@gmail.com').\n-export([expire/1]).% DO NOT EDIT.\n\n",
[
io_lib:format("expire(~p) -> ~p;~n", [Class, {PoolName, Expire}]) || {Class, {PoolName, Expire}} <- Expires
],
io_lib:format("expire(_) -> ~p.~n", [{?DEFAULT_POOL, ?DEFAULT_EXPIRE_SECONDS}])
],
	Code1 = lists:flatten(Code),
	{M, B} = dynamic_compile:from_string(Code1),
    code:load_binary(M, "", B),
    Code1.

parse_pools(Pools) ->

    % generate mcache_continuum
    io:format("[MCACHE_CONFIG] Generating mcache_continuum module ...~n", []),
    Continuums = lists:map(
					fun(PoolConfig) ->
						Name = proplists:get_value(name, PoolConfig),
						Servers = proplists:get_value(servers, PoolConfig),
						{Name, Servers}
					end,
					Pools),
    {NewPools, _} = mcache_continuum_gen:gen(Continuums, case_when),

    PoolsDict = lists:foldl(
                fun(PoolConfig, Acc) ->
                    Name = proplists:get_value(name, PoolConfig),
                    dict:store(Name, PoolConfig, Acc)
                end, dict:new(), Pools),

    % start mcache_clients
    lists:foreach(
        fun({Name, Servers, _}) ->
            %io:format("[MCACHE_CONFIG] ~p ~p~n", [Name, Pools]),
            {ok, PoolConfig} = dict:find(Name, PoolsDict),
            ConnectionCount = proplists:get_value(connection_count, PoolConfig, 10),
            lists:foreach(
                fun({Host,Port}=Addr) ->
                    io:format("[MCACHE_CONFIG] Starting mcache clients (~p) to ~p ...~n", [ConnectionCount, Addr]),
                    mcache_client_sup:start_child(Name, {Host, Port}, ConnectionCount)
                end,
                Servers)
        end,
        NewPools).
