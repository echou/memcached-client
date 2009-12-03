#!/usr/bin/env escript

main(_) ->
    io:format("~s ~s~n", [code:root_dir(),
                          code:lib_dir("erl_interface")]).
