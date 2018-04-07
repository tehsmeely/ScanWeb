-module(scanweb_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ScanServer = #{
        id => scan_server, 
        start => {scan_server, start_link, []}, 
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [scan_server]
    },
    ConfigServer = #{
        id => config_server, 
        start => {config_server, start_link, []}, 
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [config_server]
    },
    ScannedFileServer = #{
        id => scanned_file_server, 
        start => {scanned_file_server, start_link, []}, 
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [scanned_file_server]
    },
	Procs = [ScanServer, ConfigServer, ScannedFileServer],
	{ok, {{one_for_one, 1, 5}, Procs}}.
