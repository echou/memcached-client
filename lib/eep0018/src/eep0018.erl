-module(eep0018).
-author('echou327@gmail.com').

-behaviour(gen_linkedin_driver).

-export([start/0, start_link/0, encode/1, decode/1, test_decode/1]).
-export([driver_info/0]).

% callback
driver_info() ->
	{eep0018_drv, eep0018_port, eep0018_table}.

start() ->
	gen_linkedin_driver:start(?MODULE).

start_link() ->
	io:format("[DRIVER] start ~p~n", [?MODULE]),
	gen_linkedin_driver:start_link(?MODULE).


%%%% API %%%%

encode(Term) ->
	gen_linkedin_driver:control(?MODULE, 0, term_to_binary(Term)).

decode(Json) when is_binary(Json) ->
	case gen_linkedin_driver:control(?MODULE, 1, <<Json/binary, 0:8>>) of
		[] ->
			receive {json, Decoded} -> Decoded end;
		Error ->
			io:format("====== JSON ERROR ============~n", []),
			throw({invalid_json, binary_to_list(Error)})
	end;

decode(Json) when is_list(Json) ->
	decode(list_to_binary(Json)).	

