% vim:syn=erlang

-record(req, {opcode=get, seq=0, data_type=0, cas=0, extra= <<>>, key= <<>>, body= <<>>}).

-record(resp, {seq, status, opcode, data_type=0, cas=0, extra= <<>>, key= <<>>, body= <<>>, key_len=0, extra_len=0, body_len=0}).
