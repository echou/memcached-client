-module(mcache_memcached_drv_sup).
-author('echou327@gmail.com').

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

% API

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% supervisor callback
init([]) ->
    io:format("[~s] Starting~n", [?MODULE]),
    Pools = mcache_util:get_app_env(pools, []),
    Specs = lists:map(fun(Pool) ->
                    Name = proplists:get_value(name, Pool),
                    Servers = proplists:get_value(servers, Pool),
                    S1 = string:join([Addr ++ ":" ++ integer_to_list(Weight)||{Addr, Weight}<-Servers], ","),
                    { {memcached_drv, Name}, 
                      {memcached_drv, start_link, [Name, S1]}, 
                      permanent, 2000, worker, [] 
                    }
                end, Pools),
    {ok, {{one_for_one, 10, 10}, Specs}}.

