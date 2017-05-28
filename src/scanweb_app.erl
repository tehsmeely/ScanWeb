-module(scanweb_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {priv_file, scanweb, "static/html/index.html"}},
            {"/socket/", scan_socket_handler, #{activeScan=>false}},
            {"/static/js/[...]", cowboy_static, {priv_dir, scanweb, "static/js"}},
            {"/static/css/[...]", cowboy_static, {priv_dir, scanweb, "static/css"}},
            {"/scans/[...]", cowboy_static, {dir, "/home/pi/scanweb/priv/scans"}}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(scan_http_listener, 100,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    scanweb_sup:start_link().

stop(_State) ->
	ok.
