-module(mcache_test).
-author('echou327@gmail.com').

-compile(export_all).

test_get_server(P, R) ->
    F = fun(I) ->
            mcache:get_server(mcache.test, <<"key:", I:32>>)
        end,
    stresstest:start("test_get", P, R, F).

test_get(P, R, M) ->
    F = fun(I) ->
            M:get(mcache.test, <<"key:", I:32>>)
        end,
    stresstest:start("test_get", P, R, F).

test_set(P, R, M) ->
    F = fun(I) ->
            M:set(mcache.test, <<"key:", I:32>>, <<"value:", I:32, I:32, I:32, I:32>>, raw, default)
        end,
    stresstest:start("test_set", P, R, F).


test_mget(P, R, mcache) ->
    F = fun(I) ->
            mcache:mget2(mcache.test, [<<"key:", V:32>> || V <- lists:seq(I, I + 20)])
        end,
    stresstest:start("test_mget", P, R, F);
test_mget(P, R, memcached_api) ->
    F = fun(I) ->
            memcached_api:get(mcache.test, [<<"key:", V:32>> || V <- lists:seq(I, I + 20)])
        end,
    stresstest:start("test_mget", P, R, F).

