-module(static_file_config).


-export([websocket_url/0]).



websocket_url () ->
	Port = config_server:query_config(port, 8080),
	Hostname = config_server:query_config(hostname, "127.0.0.1"),
	io_lib:format("ws://~s:~w/socket/", [Hostname, Port]).
	% e.g. "ws://127.0.0.1:8080/socket/".