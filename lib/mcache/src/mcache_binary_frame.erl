-module(mcache_binary_frame).
-author('echou327@gmail.com').

-compile(bin_opt_info).

-export([initial_state/0, parse/2, encode/1]).

-include("mcache_binary_frame.hrl").

-define(HEADER_LEN, 24). % magic (0x81) + header (23 bytes)

-record(frame, {data_len=?HEADER_LEN, data= <<>>, resp=nil}).

% parse response packets

initial_state() ->
    {header, #frame{data_len=?HEADER_LEN, data= <<>>, resp=nil}, []}.

parse({State, Frame, Acc}, <<>>) ->
    { {State, Frame, []}, lists:reverse(Acc)};

parse({State, Frame, Acc}, Data) ->
    case parse_fsm(State, Frame, Data) of
        {done, Rest, Frame1} ->
            parse({header, #frame{}, [Frame1#frame.resp|Acc]}, Rest);
        {State1, Rest1, Frame1} ->
            parse({State1, Frame1, Acc}, Rest1)
    end.

parse_fsm(header, #frame{data_len=Len, data=Data}=Frame, Bin) when byte_size(Bin) >= Len ->
    <<16#81, Opcode, KeyLen:16, ExtraLen, DataType, Status:16, TotalBodyLen:32, Seq:32, CAS:64, Rest/binary>> = <<Data/binary, Bin/binary>>,
    Resp = #resp{opcode=mcache_proto:opcode(Opcode),
                 status=mcache_proto:status(Status), 
                 key_len=KeyLen, extra_len=ExtraLen, 
                 body_len=TotalBodyLen-KeyLen-ExtraLen,
                 seq=Seq, data_type=DataType, cas=CAS},
    State = if TotalBodyLen =:= 0 -> done; true -> body end,
    {State, Rest, Frame#frame{data_len=TotalBodyLen, data= <<>>, resp=Resp}};

parse_fsm(body, #frame{data_len=Len, data=Data, resp=Resp}=Frame, Bin) when byte_size(Bin) >= Len ->
    KeyLen = Resp#resp.key_len,
    ExtraLen = Resp#resp.extra_len,
    BodyLen = Resp#resp.body_len,
    <<Extra:ExtraLen/binary, Key:KeyLen/binary, Body:BodyLen/binary, Rest/binary>> = <<Data/binary, Bin/binary>>,
    {done, Rest, Frame#frame{data_len=0, data= <<>>, resp=Resp#resp{key=Key, extra=Extra, body=Body}}};

parse_fsm(State, #frame{data_len=Len, data=Data}=Frame, Bin) ->
    {State, <<>>, Frame#frame{data_len=Len-byte_size(Bin), data= <<Data/binary, Bin/binary>>}}.


% encode
encode(#req{opcode=Opcode, data_type=DataType, seq=Seq, cas=CAS, extra=Extra, key=Key, body=Body}) ->
	KeyLen = iolist_size(Key),
	BodyLen = iolist_size(Body),
	ExtraLen = iolist_size(Extra),
	TotalBodyLen = KeyLen + BodyLen + ExtraLen,
    OpcodeNum = mcache_proto:opcode(Opcode),
    [ <<16#80, OpcodeNum:8, KeyLen:16, ExtraLen:8, DataType:8, 0:16, TotalBodyLen:32, Seq:32, CAS:64>>, Extra, Key, Body ].
