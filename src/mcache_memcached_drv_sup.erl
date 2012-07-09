-module(mcache_memcached_drv_sup).
-author('echou327@gmail.com').

-behaviour(supervisor).
-export([init/1]).

-export([start_link/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->

    Pools = mcache_util:get_app_env(pools, []),

    Specs = lists:map(fun(Pool) ->
                        Name = proplists:get_value(name, Pool, generic),
                        IsBinary = proplists:get_value(binary, Pool, true),
                        Servers = proplists:get_value(servers, Pool, []),
                        S1 = string:join([Addr ++ ":" ++ integer_to_list(Weight)||{Addr, Weight}<-Servers], ","),
                        Options = [{binary, IsBinary}, {servers, S1}],
                        {{memcached_drv, Name}, 
                         {memcached_drv, start_link, [Name, Options]}, 
                         permanent, 2000, worker, []}
                 end, Pools),

    {ok, {{one_for_one, 10, 10}, Specs}}.

