-module(scan_socket_handler).

-behaviour(cowboy_websocket).

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).

-define(SCAN_DIR, "/home/jonty/Erlang/scanweb/priv/scans/").

init(Req, State) ->
	io:format("Web Socket Init!~n"),
	case cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req) of
		undefined ->
			io:format("Web Socket - no subprotocols!~n"),
			{cowboy_websocket, Req, State};
		Subprotocols ->
			case lists:keymember(<<"mqtt">>, 1, Subprotocols) of
				true ->
					io:format("Web Socket - mqtt!~n"),
					Req2 = cowboy_req:set_resp_header(<<"sec-websocket-protocol">>,
						<<"mqtt">>, Req),
					{cowboy_websocket, Req2, State};
				false ->
					io:format("Web Socket - fail - stop!~n"),
					{stop, Req, State}
			end
	end.


websocket_init(State) ->
	io:format("Web Socket websocket_init!~n"),
	{Status, Info} = case scan_server:get_is_active() of
		{true, Info0} ->
			{"ACTIVE", Info0};
		false ->
			{"INACTIVE", none}
	end,
	Resp = #{
		<<"TYPE">> => <<"SCANNER">>,
		<<"STATUS">> => list_to_binary(Status),
		<<"INFO">> => Info
	},
	{reply, {text, jiffy:encode(Resp)}, State}.

% websocket_handle({text, <<"SCAN">>}, State) ->
% 	io:format("Websocket got Scan request~n"),
% 	scan_server:get_scan(),
% 	{reply, {text, <<"SCAN_STARTING">>}, State};
websocket_handle({text, <<"PING">>}, State) ->
	{reply, {text, <<"PONG">>}, State};
websocket_handle({text, FrameData}=Frame, State) ->
	io:format("Got Frame: ~p~n", [Frame]),
	EJson = jiffy:decode(FrameData, [return_maps]),
	io:format("Parsed to JSON: ~p~n", [EJson]),
	Action = maps:get(<<"ACTION">>, EJson),
	{Type, Success, ScanInfo} = case Action of
		<<"SCAN">> ->
			%unpack optional args
			Resolution = maps:get(<<"RESOLUTION">>, EJson, none),
			Mode = maps:get(<<"MODE">>, EJson, none),
			ScanInfo0 = scan_server:get_scan([{resolution, Resolution}, {mode, Mode}]),
			{<<"RESPONSE">>, <<"TRUE">>, ScanInfo0};
		<<"GET_FILES">> ->
			self() ! sendFiles,
			{<<"GET_FILES_RESPONSE">>, <<"TRUE">>, none};
		_ ->
			{<<"RESPONSE">>, <<"FALSE">>, none}
	end,
	Resp = #{
		<<"TYPE">> => Type,
		<<"SUCCESS">> => Success,
		<<"INFO">> => ScanInfo
	},
	{reply, {text, jiffy:encode(Resp)}, State}.

websocket_info(sendFiles, State) ->
	io:format("handler got sendFiles message~n"),
	Resp = case scanned_file_server:get_snapshot() of
		{error, no_files} ->
			#{
				<<"TYPE">> => <<"FILES">>,
				<<"FILES_EXIST">> => <<"FALSE">>
			};
		{ok, {Latest, All}} ->
			#{
				<<"TYPE">> => <<"FILES">>,
				<<"FILES_EXIST">> => <<"TRUE">>,
				<<"LATEST_IMAGE">> => Latest,
				<<"IMAGE_LIST">> => All
			}
	end,
	%Resp = list_to_binary(io_lib:format("Fail: Reason ~s", [Reason])),
	JResp = jiffy:encode(Resp),
	{reply, {text, JResp}, State};
websocket_info({scan, fail, Reason} , State) ->
	io:format("handler got fail message~n"),
	Resp = #{
		<<"TYPE">> => <<"STATUS">>,
		<<"SUCCESS">> => <<"FALSE">>,
		<<"REASON">> => list_to_binary(Reason)
	},
	%Resp = list_to_binary(io_lib:format("Fail: Reason ~s", [Reason])),
	JResp = jiffy:encode(Resp),
	{reply, {text, JResp}, State};
websocket_info({scan, success, Filename}, State) ->
	io:format("handler got success message~n"),
	Resp = #{
		<<"TYPE">> => <<"STATUS">>,
		<<"SUCCESS">> => <<"TRUE">>,
		<<"FILENAME">> => list_to_binary(Filename)
	},
	JResp = jiffy:encode(Resp),
	%Resp = list_to_binary(io_lib:format("Success: Filename ~s", [Filename])),
	{reply, {text, JResp}, State};
websocket_info(_Info, State) ->
	{ok, State}.