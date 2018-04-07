-module(scanweb_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    io:format("Starting...~n"),
    scanweb_sup:start_link(),
    io:format("Servers Started~n"),
    timer:sleep(1000),
    io:format("startup: ~p~n",[config_server:query_config(port, 8080)]),
    static_file_manager:compile_and_render(),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {priv_file, scanweb, "static/html/index.html"}},
            {"/socket/", scan_socket_handler, #{activeScan=>false}},
            {"/static/js/[...]", cowboy_static, {priv_dir, scanweb, "static/js"}},
            {"/static/css/[...]", cowboy_static, {priv_dir, scanweb, "static/css"}},
            {"/scans/[...]", cowboy_static, {priv_dir, scanweb, "scans"}},
            {"/api/", scan_rest_api_handler, #{activeScan=>false}}
        ]}
    ]),
    Port = config_server:query_config(port, 8080),
    {ok, _} = cowboy:start_clear(scan_http_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ).

stop(_State) ->
	ok.

