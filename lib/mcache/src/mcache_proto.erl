-module(mcache_proto).
-author('echou327@gmail.com').

-export([status/1, opcode/1]).

status(16#0000) -> ok;
status(16#0001) -> not_found;
status(16#0002) -> exists;
status(16#0003) -> too_large;
status(16#0004) -> badarg;
status(16#0005) -> not_stored;
status(16#0006) -> non_numeric.

opcode(0) -> get;
opcode(1) -> set;
opcode(2) -> add;
opcode(3) -> replace;
opcode(4) -> delete;
opcode(5) -> incr;
opcode(6) -> decr;
opcode(7) -> quit;
opcode(8) -> flush;
opcode(9) -> getq;
opcode(10) -> noop;
opcode(11) -> version;
opcode(12) -> getk;
opcode(13) -> getkq;
opcode(14) -> append;
opcode(15) -> prepend;
opcode(16) -> stat;
opcode(17) -> setq;
opcode(18) -> addq;
opcode(19) -> replaceq;
opcode(20) -> deleteq;
opcode(21) -> incrq;
opcode(22) -> decrq;
opcode(23) -> quitq;
opcode(24) -> flushq;
opcode(25) -> appendq;
opcode(26) -> prependq;

opcode(get) -> 0;
opcode(set) -> 1;
opcode(add) -> 2;
opcode(replace) -> 3;
opcode(delete) -> 4;
opcode(incr) -> 5;
opcode(decr) -> 6;
opcode(quit) -> 7;
opcode(flush) -> 8;
opcode(getq) -> 9;
opcode(noop) -> 10;
opcode(version) -> 11;
opcode(getk) -> 12;
opcode(getkq) -> 13;
opcode(append) -> 14;
opcode(prepend) -> 15;
opcode(stat) -> 16;
opcode(setq) -> 17;
opcode(addq) -> 18;
opcode(replaceq) -> 19;
opcode(deleteq) -> 20;
opcode(incrq) -> 21;
opcode(decrq) -> 22;
opcode(quitq) -> 23;
opcode(flushq) -> 24;
opcode(appendq) -> 25;
opcode(prependq) -> 26.

