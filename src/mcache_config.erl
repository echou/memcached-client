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
	io:format("[~s] init~n", [?MODULE]),
	process_flag(trap_exit, true),
	
	parse_expires(Expires),
	%parse_pools(Pools),
	{ok, #state{expires=Expires, initial_expires=Expires}}.


handle_call({update_expires, Info}, _From, #state{expires=OldExpires, initial_expires=InitialExpires}=State) ->
	NewExpires = case Info of
					restore ->		
						% restore to initial config
						InitialExpires;
					{assign, Expires} -> 
						% set a new set of config
						Expires;
					{delete, Class} ->
						proplists:delete(Class, OldExpires);
					{set, Class, {PoolName, Expiry}} -> 
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
	io:format("[~s] terminate() reason: ~p~n", [?MODULE, Reason]).

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
	io:format("[~s] Generating mcache_expires module ...~n", [?MODULE]),

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
    io:format("~p~n", [code:load_binary(M, "", B)]),
    Code1.


normalize_server({{_,_,_,_}=Addr, Port, Weight}) ->
    {Addr, Port, Weight};
normalize_server({Addr, Port, Weight}) when is_list(Addr) ->
    {Addr1, _} = normalize_addr(Addr),
    {Addr1, Port, Weight};
normalize_server({Addr, Weight}) ->
    {Addr1, Port1} = normalize_addr(Addr),
    {Addr1, Port1, Weight}.

normalize_addr(Addr) when is_list(Addr) ->
    case string:tokens(Addr, ":") of
        [IP] ->
            {ok, Addr1} = inet:getaddr(IP, inet),
            {Addr1, 11211};
        [IP,PortStr|_] ->
            {ok, Addr1} = inet:getaddr(IP, inet),
            Port1 = case string:to_integer(PortStr) of
                        {Port, []} -> Port;
                        _ -> 11211
                    end,
            {Addr1, Port1};
        _ ->
            {{127,0,0,1}, 11211}
    end.
