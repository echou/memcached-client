% vim:syn=erlang

-record(req, {opcode=get, seq=0, data_type=0, cas=0, extra= <<>>, key= <<>>, body= <<>>}).

-record(resp, {seq, status, opcode, data_type=0, cas=0, extra= <<>>, key= <<>>, body= <<>>, key_len=0, extra_len=0, body_len=0}).

-define(get, 0).
-define(set, 1).
-define(add, 2).
-define(replace, 3).
-define(delete, 4).
-define(incr, 5).
-define(decr, 6).
-define(quit, 7).
-define(flush, 8).
-define(getq, 9).
-define(noop, 10).
-define(version, 11).
-define(getk, 12).
-define(getkq, 13).
-define(append, 14).
-define(prepend, 15).
-define(stat, 16).
-define(setq, 17).
-define(addq, 18).
-define(replaceq, 19).
-define(deleteq, 20).
-define(incrq, 21).
-define(decrq, 22).
-define(quitq, 23).
-define(flushq, 24).
-define(appendq, 25).
-define(prependq, 26).

-define(ok, 0).
-define(not_found, 1).
-define(exists, 2).
-define(too_large, 3).
-define(badarg, 4).
-define(not_stored, 5).
-define(non_numeric, 6).
