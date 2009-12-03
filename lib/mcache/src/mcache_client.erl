-module(mcache_client).
-author('echou327@gmail.com').

%-compile([inline, native, {hipe,o3}]).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,code_change/3,terminate/2]).
-export([mc_get/2, ab_get/2, ab_mget/2, mc_set/5, ab_set/5, mc_delete/2, ab_delete/2]).
-export([test_get/4, test_get2/2]).

-include_lib("kernel/src/inet_int.hrl").

-define(PG2_GROUP_TAG, ?MODULE).
-define(SOCK_OPTS, [binary, {active, true}, {delay_send, false}, {nodelay, true}, {packet, raw}]).
-define(CONNECT_TIMEOUT, 1000).
-define(RECONNECT_AFTER, 2000).

-record(state, {addr, seq=0, buffer, pendings, sock=not_connected, connecting, mgets}).
-record(req, {opcode=get, data_type=0, cas=0, extra= <<>>, key= <<>>, body= <<>>}).
-record(resp, {seq, status, opcode, data_type=0, cas=0, extra= <<>>, key= <<>>, body= <<>>}).

start_link({Host, Port}) ->
    gen_server:start_link(?MODULE, [{Host, Port}], []).

init([{Host, Port}=Server]) ->
    process_flag(trap_exit, true),

    pg2:create({?PG2_GROUP_TAG, Server}),
    pg2:join({?PG2_GROUP_TAG, Server}, self()),

    {ok, Sock, Ref} = async_connect(Host, Port, ?SOCK_OPTS, ?CONNECT_TIMEOUT),

    {ok, #state{sock=not_connected,
                addr={Host, Port},
                seq=0,
                buffer= <<>>,
                pendings=dict:new(),
                mgets=dict:new(),
                connecting={Sock, Ref}}}.

% HANDLE_CALL

handle_call({mc, Req}, _From, #state{sock=not_connected}=State) ->
    {reply, {error, not_connected}, State};

handle_call({mc, Req}, From, State) ->
    case send_wrapper(Req, From, State) of
        {ok, NewState} ->
            {noreply, NewState};
        {Any, NewState} ->
            {reply, {error, Any}, NewState}
    end;

handle_call({mc_ab, Req}, From, State) ->
    case send_wrapper(Req, From, State) of
        {ok, NewState} ->
            {reply, pending, NewState};
        {Any, NewState} ->
            {reply, {error, Any}, NewState}
    end;

handle_call(_Req, _From, State) ->
    {noreply, State}.

send_wrapper({mget, Keys}=Req, From, #state{sock=Sock, seq=Seq, pendings=Ps, mgets=Ms}=State) ->
    case (catch send_req(Sock, Seq, Req)) of
        true ->
            ItemDict = lists:foldl(fun(K, Acc) -> dict:store(K, undefined, Acc) end, dict:new(), Keys),
            {ok, State#state{seq=Seq+1,
                        pendings=dict:store(Seq, {From, app_util:now()}, Ps),
                        mgets=dict:store(Seq, ItemDict, Ms)}};
        _ ->
            {not_sent, State}
    end;
send_wrapper(Req, From, #state{sock=Sock, seq=Seq, pendings=Ps}=State) ->
    case (catch send_req(Sock, Seq, Req)) of
        true ->
            {ok, State#state{seq=Seq+1, pendings=dict:store(Seq, {From, app_util:now()}, Ps)}};
        _ ->
            {not_sent, State}
    end.

% HANDLE_CAST

handle_cast({mc, Req}, #state{sock=not_connected}=State) ->
    {noreply, State};

handle_cast({mc, Req}, #state{sock=Sock, seq=Seq}=State) ->
    case (catch send_req(Sock, Seq, Req)) of
        ok ->
            {noreply, State#state{seq=Seq+1}};
        _ ->
            {noreply, State}
    end;

handle_cast(_Req, State) ->
    {noreply, State}.

% HANDLE_INFO

handle_info({inet_async, Sock, Ref, Status}, #state{connecting={Sock, Ref}}=State) ->
    %error_logger:info_msg("inet_async: ~p, ~p, ~p~n", [Sock, Ref, Status]),
    case Status of
        ok ->
            {noreply, State#state{sock=Sock, connecting=undefined}};
        _ ->
            gen_tcp:close(Sock),
            erlang:send_after(?RECONNECT_AFTER, self(), reconnect),
            {noreply, State#state{sock=not_connected, connecting=undefined}}
    end;

handle_info({tcp_closed, Sock}, #state{sock=Sock, connecting=undefined}=State) ->
    erlang:send_after(?RECONNECT_AFTER, self(), reconnect),
    {noreply, State#state{sock=not_connected, connecting=undefined,buffer= <<>>}, hibernate};

handle_info(reconnect, #state{addr={Host, Port}}=State) ->
    {ok, Sock, Ref} = async_connect(Host, Port, ?SOCK_OPTS, ?CONNECT_TIMEOUT),
    {noreply, State#state{sock=not_connected, connecting={Sock, Ref}}, hibernate};

handle_info({tcp, Sock, Data}, #state{sock=Sock,buffer=Buf,pendings=Pendings, mgets=Mgets}=State) ->
    {NewBuf, Resps} = do_parse(<<Buf/binary,Data/binary>>, []),
    case Resps of
        [] ->
            {noreply, State#state{buffer=NewBuf}};
        [_|_] ->
            {NewPendings, NewMgets} = lists:foldl(fun handle_resp/2, {Pendings, Mgets}, Resps),
            {noreply, State#state{buffer=NewBuf,pendings=NewPendings,mgets=NewMgets}}
    end;

handle_info(Msg, State) ->
    %error_logger:info_msg("handle_info: ~p~n", [Msg]),
    {noreply, State}.

% MISC CALLBACKS

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, State) ->
    ok.


% Handles mget sequences (getkq, getkq, ..., noop)
handle_resp(#resp{opcode=getkq, seq=Seq}=Resp, {Ps, Ms}) ->
    NewMs = case dict:find(Seq, Ms) of
                error -> % not found
                    Ms;
                {ok, ItemDict} ->
                    case process_resp(Resp) of
                        {ok, {Key, Value}} ->
                            dict:store(Seq, dict:store(Key, Value, ItemDict), Ms);
                        _ ->
                            Ms
                    end
            end,

    {Ps, NewMs};

handle_resp(#resp{opcode=noop,seq=Seq}=Resp, {Ps, Ms}) ->
    case dict:find(Seq, Ms) of
        error ->
            Ms;
        {ok, ItemDict} ->
            case dict:find(Seq, Ps) of
                {ok, {From, _Time}} ->
                    gen_server:reply(From, {mget, ItemDict});
                error ->
                    ok
            end
    end,

    {dict:erase(Seq, Ps), dict:erase(Seq, Ms)};

handle_resp(#resp{seq=Seq}=Resp, {Ps, Ms}) ->
    case dict:find(Seq, Ps) of
        error ->
            ignore;
        {ok, {From, _Time}} ->
            Result = case (catch process_resp(Resp)) of
                        {ok, Any} -> Any;
                        {'EXIT', Reason} -> {error, Reason};
                        Status -> Status
                    end,
            gen_server:reply(From, Result)
    end,
    {dict:erase(Seq, Ps), Ms}.

% PUBLIC APIS

mc_get(Server, Key) ->
    gen_server:call(get_client_pid(Server), {mc, {getk, Key}}).

ab_get(Server, Key) ->
    gen_server:call(get_client_pid(Server), {mc_ab, {getk, Key}}).

mc_mget(Server, Keys) ->
    gen_server:call(get_client_pid(Server), {mc, {mget, Keys}}).

ab_mget(Server, Keys) ->
    gen_server:call(get_client_pid(Server), {mc_ab, {mget, Keys}}).

mc_set(Server, Key, Value, Flags, Expiry) ->
	gen_server:call(get_client_pid(Server), {mc, {set, Key, Value, Flags, Expiry}}).

ab_set(Server, Key, Value, Flags, Expiry) ->
	gen_server:cast(get_client_pid(Server), {mc, {set, Key, Value, Flags, Expiry}}).

mc_delete(Server, Key) ->
	gen_server:call(get_client_pid(Server), {mc, {delete, Key}}).

ab_delete(Server, Key) ->
	gen_server:cast(get_client_pid(Server), {mc, {delete, Key}}).


% internal apis

get_client_pid(Server) ->
    case app_util:pg2_get_closest_pid({?PG2_GROUP_TAG, Server}) of
        {error, {no_process, _Reason}} ->
            exit(no_process);
        Pid when is_pid(Pid) ->
            %error_logger:info_msg("get_client_pid ~p ~p~n", [Server, Pid]),
            Pid
    end.

async_connect({A,B,C,D}=_Addr, Port, Opts, Time) ->
    case inet:connect_options(Opts, inet) of
        {error, Reason} ->
            exit(Reason);
        {ok, #connect_opts{fd=Fd, ifaddr=BAddr={_,_,_,_}, port=BPort, opts=SockOpts}} ->
            case inet:open(Fd,BAddr,BPort,SockOpts,tcp,inet,?MODULE) of
                {ok, S} ->
                    prim_inet:async_connect(S, {A,B,C,D}, Port, Time);
                Error ->
                    Error
            end;
        {ok, _} = Any ->
            exit(badarg)
    end.

do_parse(<<16#81, Opcode, KeyLen:16, ExtraLen, DataType, Status:16, TotalBodyLen:32, Seq:32, CAS:64, TotalBody:TotalBodyLen/binary, Rest/binary>>, Acc) ->
    <<Extra:ExtraLen/binary, Key:KeyLen/binary, Body/binary>> = TotalBody,
    Resp= #resp{seq=Seq,
                opcode=mcache_proto:opcode(Opcode),
                status=mcache_proto:status(Status),
                data_type=DataType,
                cas=CAS,
                extra=Extra,
				key=Key,
                body=Body},
    do_parse(Rest, [Resp|Acc]);
do_parse(Data, Acc) ->
    {Data, lists:reverse(Acc)}.

send_req(Sock, Seq, {mget, Keys}) ->
    Reqs = [ #req{opcode=getkq, key=K} || K <- Keys ],
    do_send_req(Sock, Seq, lists:reverse([#req{opcode=noop}|Reqs]));

send_req(Sock, Seq, noop) ->
	do_send_req(Sock, Seq, #req{opcode=noop});

send_req(Sock, Seq, version) ->
	do_send_req(Sock, Seq, #req{opcode=version});

send_req(Sock, Seq, {delete, Key}) ->
	do_send_req(Sock, Seq, #req{opcode=delete, key=Key});

send_req(Sock, Seq, {flush, Expiry}) ->
	do_send_req(Sock, Seq, #req{opcode=flush, extra= <<Expiry:32>>});
send_req(Sock, Seq, flush) ->
	do_send_req(Sock, Seq, #req{opcode=flush});

send_req(Sock, Seq, {Op, Key}) when Op=:=get;Op=:=getk;Op=:=getkq ->
	do_send_req(Sock, Seq, #req{opcode=Op, key=Key});

send_req(Sock, Seq, {Op, Key, Value, Flags, Expiry}) when Op=:=set;Op=:=add;Op=:=replace ->
	do_send_req(Sock, Seq, #req{opcode=Op, key=Key, body=Value, extra= <<Flags:32, Expiry:32>>});

send_req(Sock, Seq, {Op, Key, Delta, Initial, Expiry}) when Op=:=incr;Op=:=decr ->
	do_send_req(Sock, Seq, #req{opcode=Op, key=Key, extra= <<Delta:64, Initial:64, Expiry:32>>}).



process_resp(#resp{opcode=noop}) ->
    {ok, noop};

process_resp(#resp{opcode=get, status=Status, extra=Extra, body=Value}) ->
    case Status of
        ok ->
            <<Flags:32>> = Extra,
            {ok, {Value, Flags}};
        _ -> {ok, undefined}
    end;

process_resp(#resp{opcode=getk, status=Status, extra=Extra, key=Key, body=Value}) ->
    case Status of
        ok ->
            <<Flags:32>> = Extra,
            {ok, {Key, {Value, Flags}}};
        _ -> {ok, {Key, undefined}}
    end;

process_resp(#resp{opcode=getkq, status=Status, extra=Extra, key=Key, body=Value}) ->
    case Status of
        ok ->
            <<Flags:32>> = Extra,
            {ok, {Key, {Value, Flags}}};
        _ -> {ok, {Key, undefined}}
    end;

process_resp(#resp{opcode=incr, status=ok, body= <<Value:64>>}) ->
	{ok, Value};

process_resp(#resp{opcode=decr, status=ok, body= <<Value:64>>}) ->
	{ok, Value};

process_resp(#resp{opcode=version, status=ok, body=Body}) ->
	{ok, binary_to_list(Body)};

process_resp(#resp{opcode=_, status=Status}) ->
	Status.

gen_req(Seq, #req{opcode=Opcode,
                  data_type=DataType,
                  cas=CAS,
                  extra=Extra,
                  key=Key,
                  body=Body}=_Req) ->
	KeyLen = iolist_size(Key),
	BodyLen = iolist_size(Body),
	ExtraLen = iolist_size(Extra),
	TotalBodyLen = KeyLen + BodyLen + ExtraLen,
    [16#80,						% MAGIC
     mcache_proto:opcode(Opcode),	% Opcode
     <<KeyLen:16,					% Key length
       ExtraLen:8,				% Extra length
       DataType:8,				% Data type
       0:16,						% Reserved
       TotalBodyLen:32,			% Total body length
       Seq:32,					% Opaque (as Seq)
       CAS:64>>,					% CAS
     Extra,
     Key,
     Body].

do_send_req(Sock, Seq, [_,_|_]=Reqs) ->
    erlang:port_command(Sock, [ gen_req(Seq, Req) || Req <- Reqs ]);
do_send_req(Sock, Seq, [Req]) ->
    erlang:port_command(Sock, gen_req(Seq, Req));
do_send_req(Sock, Seq, Req) ->
    erlang:port_command(Sock, gen_req(Seq, Req)).

test_get(P, R, S, K) ->
    F = fun(_) -> mc_get(S, K) end,
    stresstest:start("mcache_client:get", P, R, F).

test_get2(P, R) ->
    F = fun(_) ->
            memcached_api:get(webqq, <<"439311075:friends">>)
        end,
    stresstest:start("memcached_client:get", P, R, F).
