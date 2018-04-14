-module(scan_rest_api_handler).

-behaviour(cowboy_rest).

-export([init/2, content_types_provided/2, do_scan/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

content_types_provided(Req, State) ->
    {[
	    {<<"text/html">>, do_scan},
	    {<<"application/json">>, do_scan},
	    {<<"text/plain">>, do_scan}
    ], Req, State}.


do_scan( Req, State ) ->
	Queries = cowboy_req:parse_qs(Req),
	Opts = convert_keys_to_atom(Queries),
	scan_server:get_scan(Opts),
	receive
		{scan, success, Filename} ->
			{scan_success(Filename), Req, State};
		M ->
			io:format("Received other msg: ~p~n", [M])
	end.

scan_success(Filename) ->
	io:format("handler got success message~n"),
	Resp = #{
		<<"TYPE">> => <<"STATUS">>,
		<<"SUCCESS">> => <<"TRUE">>,
		<<"FILENAME">> => list_to_binary(Filename)
	},
	jiffy:encode(Resp).

convert_keys_to_atom(KeyList)->
	convert_keys_to_atom(KeyList, []).	
convert_keys_to_atom([], ConvertedList) ->
	ConvertedList;
convert_keys_to_atom([H|L], ConvertedList) ->
	case H of
		{BinKey, Value} ->
			Key = binary_to_atom(BinKey, utf8),
			convert_keys_to_atom(L, [{Key, Value}|ConvertedList]);
		_ ->
			convert_keys_to_atom(L, ConvertedList)
	end.
