-module(mcache_client).
-author('echou327@gmail.com').

-compile([inline, bin_opt_info]).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,code_change/3,terminate/2]).
-export([mc_get/2, ab_get/2, mc_mget/2, ab_mget/3, mc_set/5, ab_set/5, mc_delete/2, ab_delete/2]).


-include_lib("kernel/src/inet_int.hrl").

-define(PG2_GROUP_TAG, ?MODULE).
-define(SOCK_OPTS, [binary, {active, true}, {delay_send, false}, {nodelay, true}, {packet, raw}]).
-define(CONNECT_TIMEOUT, 2000).
-define(RECONNECT_AFTER, 5000).

-define(DICT, pdict).

-record(state, {addr, seq=0, parser, pendings, sock=not_connected, connecting}).

-include("mcache_binary_frame.hrl").

-record(pending, {from, time}).
-record(mget_pending, {from, time, num_keys, items}).

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
                parser=mcache_binary_frame:initial_state(),
                pendings=?DICT:new(),
                connecting={Sock, Ref}}}.

% HANDLE_CALL

handle_call({mc, _Req}, _From, #state{sock=not_connected}=State) ->
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

% HANDLE_CAST

handle_cast({mc, _Req}, #state{sock=not_connected}=State) ->
    {noreply, State};

handle_cast({mc_ab, {mget, From, Keys}}, State) ->
    {_, State1} = send_wrapper({mget, Keys}, From, State),
    {noreply, State1};

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
            {noreply, socket_close(State), hibernate}
    end;

handle_info({tcp_closed, Sock}, #state{sock=Sock}=State) ->
    catch gen_tcp:close(Sock),
    {stop, tcp_closed, State};

handle_info(reconnect, #state{addr={Host, Port}}=State) ->
    {ok, Sock, Ref} = async_connect(Host, Port, ?SOCK_OPTS, ?CONNECT_TIMEOUT),
    {noreply, State#state{sock=not_connected, connecting={Sock, Ref}}, hibernate};

handle_info({tcp, Sock, Data}, #state{sock=Sock,parser=Parser,pendings=Pendings}=State) ->
    {NewParser, Resps} = mcache_binary_frame:parse(Parser, Data),
    case Resps of
        [] ->
            {noreply, State#state{parser=NewParser}};
        [_|_] ->
            NewPendings = lists:foldl(fun handle_one_resp/2, Pendings, Resps),
            {noreply, State#state{parser=NewParser,pendings=NewPendings}}
    end;

handle_info({tcp_old, Sock, Data}, #state{sock=Sock,pendings=Pendings}=State) ->
    Resps = parse(Data),
    %error_logger:info_msg("recv: ~p~n", [Resps]),
    case Resps of
        [] ->
            {noreply, State};
        [_|_] ->
            NewPendings = lists:foldl(fun handle_one_resp/2, Pendings, Resps),
            {noreply, State#state{pendings=NewPendings}}
    end;
    
    

handle_info(_Msg, State) ->
    %error_logger:info_msg("handle_info: ~p~n", [_Msg]),
    {noreply, State}.

% MISC CALLBACKS

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%%% internal functions

pending_create({mget, Keys}, From) ->
    #mget_pending{from=From, time=erlang:now(), num_keys=length(Keys), items=nil};
pending_create(_Req, From) ->
    #pending{from=From, time=erlang:now()}.

send_wrapper(Req, From, #state{sock=Sock, seq=Seq, pendings=Pendings}=State) ->
    case (catch send_req(Sock, Seq, Req)) of
        true ->
            P = pending_create(Req, From), 
            {ok, State#state{seq=Seq+1, pendings=?DICT:store(Seq,P,Pendings)}};
        _Any ->
            io:format("send_wrapper ~p~n", [_Any]),
            {not_sent, State}
    end.

socket_close(#state{sock=not_connected, pendings=Pendings}=State) ->
    flush_pendings(Pendings, {error, closed}),
    erlang:send_after(?RECONNECT_AFTER, self(), reconnect),
    State#state{connecting=undefined, pendings=?DICT:new(), parser=mcache_binary_frame:initial_state()};
socket_close(#state{sock=Sock}=State) when is_port(Sock) ->
    catch gen_tcp:close(Sock),
    socket_close(State#state{sock=not_connected}).

prepend_list(Key, Value, nil) ->
    [{Key,Value}];
prepend_list(Key, Value, L) ->
    [{Key,Value}|L].

parse(Bin) ->
    L = 
        case erlang:get(current_data) of
            undefined ->
                parse_resp(Bin, []);
            Data ->
                parse_resp(<<Data/binary, Bin/binary>>, [])
        end,
    lists:reverse(L).

parse_resp(<<>>, L) ->
    erlang:erase(current_data),
    L;
parse_resp(<<16#81, Opcode, KeyLen:16, ExtraLen, _DataType, Status:16, TotalBodyLen:32, Seq:32, CAS:64, Body:TotalBodyLen/binary, Rest/binary>>, L) ->
    parse_resp(Rest, [build_resp(Opcode, Status, Seq, CAS, KeyLen, ExtraLen, Body)|L]);

parse_resp(Bin, L) ->
    case erlang:get(current_data) of
        undefined ->
            erlang:put(current_data, Bin),
            L;
        Data ->
            parse_resp(<<Data/binary, Bin/binary>>, L)
    end.

build_resp(Opcode, Status, Seq, CAS, KeyLen, ExtraLen, Body) ->
    <<Extra:ExtraLen/binary, Key:KeyLen/binary, Data/binary>> = Body,
    #resp{opcode=Opcode,
          status=Status,
          seq=Seq,
          cas=CAS,
          key=Key,
          extra=Extra,
          body=Data}.

% Handles mget sequences (getkq, getkq, ..., noop)
handle_one_resp(#resp{opcode=?getkq, seq=Seq}=Resp, Pendings) ->
    case ?DICT:find(Seq, Pendings) of
        {ok, #mget_pending{items=Items}=P} ->
            case decode_resp(Resp) of
                {ok, {Key, Value}} ->
                    ?DICT:store(Seq, P#mget_pending{items=prepend_list(Key,Value,Items)}, Pendings);
                _ ->
                    Pendings
            end;
        _ -> % normal pendings
            Pendings
    end;

handle_one_resp(#resp{opcode=?noop,seq=Seq}, Pendings) ->
    case ?DICT:find(Seq, Pendings) of
        {ok, #mget_pending{from=From, num_keys=NumKeys, items=Items}} ->
            gen_server:reply(From, {mget, NumKeys, Items});
        _ ->
            ok
    end,
    ?DICT:erase(Seq, Pendings);


handle_one_resp(#resp{seq=Seq}=Resp, Pendings) ->
    case ?DICT:find(Seq, Pendings) of
        {ok, #pending{from=From}} ->
            Result = case (catch decode_resp(Resp)) of
                        {ok, Any} -> Any;
                        {'EXIT', Reason} -> {error, Reason};
                        Status -> Status
                    end,
            gen_server:reply(From, Result);
        _ ->
            ok
    end,
    ?DICT:erase(Seq, Pendings).

% TODO modification not done yet!
flush_pendings(Pendings, Result) ->
    dict:map(
        fun(_Seq, #pending{from=From}) ->
            gen_server:reply(From, Result),
            ok;
        (_Seq, #mget_pending{from=From, items=Items}) ->
            gen_server:reply(From, {mget, Items}),
            ok;
        (_, _) ->
            ok
        end,
        Pendings).


% internal apis

get_client_pid(Server) ->
    case pg2:get_closest_pid({?PG2_GROUP_TAG, Server}) of
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
        {ok, _} ->
            exit(badarg)
    end.

do_send_req(Sock, [_|_]=Reqs) ->
    erlang:port_command(Sock, [ mcache_binary_frame:encode(Req) || Req <- Reqs ]);
do_send_req(Sock, Req) ->
    erlang:port_command(Sock, mcache_binary_frame:encode(Req)).

send_req(Sock, Seq, {mget, Keys}) ->
    Reqs = [ #req{opcode=?getkq, seq=Seq, key=K} || K <- Keys ],
    do_send_req(Sock, lists:reverse([#req{opcode=?noop, seq=Seq}|Reqs]));

send_req(Sock, Seq, version) ->
    do_send_req(Sock, #req{opcode=?version, seq=Seq});

send_req(Sock, Seq, {delete, Key}) ->
    do_send_req(Sock, #req{opcode=?delete, seq=Seq, key=Key});

send_req(Sock, Seq, {flush, Expiry}) ->
    do_send_req(Sock, #req{opcode=?flush, seq=Seq, extra= <<Expiry:32>>});
send_req(Sock, Seq, flush) ->
    do_send_req(Sock, #req{opcode=?flush, seq=Seq});

send_req(Sock, Seq, {getk, Key}) ->
    do_send_req(Sock, #req{opcode=?getk, seq=Seq, key=Key});

send_req(Sock, Seq, {set, Key, Value, Flags, Expiry}) ->
    do_send_req(Sock, #req{opcode=?set, seq=Seq, key=Key, body=Value, extra= <<Flags:32, Expiry:32>>});

send_req(Sock, Seq, {add, Key, Value, Flags, Expiry}) ->
    do_send_req(Sock, #req{opcode=?add, seq=Seq, key=Key, body=Value, extra= <<Flags:32, Expiry:32>>});

send_req(Sock, Seq, {replace, Key, Value, Flags, Expiry}) ->
    do_send_req(Sock, #req{opcode=?replace, seq=Seq, key=Key, body=Value, extra= <<Flags:32, Expiry:32>>});

send_req(Sock, Seq, {incr, Key, Delta, Initial, Expiry}) ->
    do_send_req(Sock, #req{opcode=?incr, seq=Seq, key=Key, extra= <<Delta:64, Initial:64, Expiry:32>>});

send_req(Sock, Seq, {decr, Key, Delta, Initial, Expiry}) ->
    do_send_req(Sock, #req{opcode=?decr, seq=Seq, key=Key, extra= <<Delta:64, Initial:64, Expiry:32>>}).


decode_resp(#resp{opcode=?noop}) ->
    {ok, noop};

decode_resp(#resp{opcode=?getk, status=Status, extra=Extra, key=Key, body=Value}) ->
    case Status of
        ?ok ->
            <<Flags:32>> = Extra,
            {ok, {Key, {Value, Flags}}};
        _ -> 
            {ok, {Key, undefined}}
    end;

decode_resp(#resp{opcode=?getkq, status=Status, extra=Extra, key=Key, body=Value}) ->
    case Status of
        ?ok ->
            <<Flags:32>> = Extra,
            {ok, {Key, {Value, Flags}}};
        _ -> 
            {ok, {Key, undefined}}
    end;

decode_resp(#resp{opcode=?incr, status=?ok, body= <<Value:64>>}) ->
	{ok, Value};

decode_resp(#resp{opcode=?decr, status=?ok, body= <<Value:64>>}) ->
	{ok, Value};

decode_resp(#resp{opcode=?version, status=?ok, body=Body}) ->
	{ok, binary_to_list(Body)};

decode_resp(#resp{opcode=_, status=Status}) ->
	mcache_proto:status(Status).


% ======================================================= 
%                    PUBLIC API
% =======================================================

mc_get(Server, Key) ->
    gen_server:call(get_client_pid(Server), {mc, {getk, Key}}).

ab_get(Server, Key) ->
    gen_server:call(get_client_pid(Server), {mc_ab, {getk, Key}}).

mc_mget(Server, Keys) ->
    gen_server:call(get_client_pid(Server), {mc, {mget, Keys}}).

ab_mget(Server, Ref, Keys) ->
    % gen_server:call(get_client_pid(Server), {mc_ab, {mget, Keys}}).
    gen_server:cast(get_client_pid(Server), {mc_ab, {mget, {self(), Ref}, Keys}}).


mc_set(Server, Key, Value, Flags, Expiry) ->
	gen_server:call(get_client_pid(Server), {mc, {set, Key, Value, Flags, Expiry}}).

ab_set(Server, Key, Value, Flags, Expiry) ->
	gen_server:cast(get_client_pid(Server), {mc, {set, Key, Value, Flags, Expiry}}).

mc_delete(Server, Key) ->
	gen_server:call(get_client_pid(Server), {mc, {delete, Key}}).

ab_delete(Server, Key) ->
	gen_server:cast(get_client_pid(Server), {mc, {delete, Key}}).
