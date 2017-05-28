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
	Procs = [ScanServer],
    %Procs = [],
	{ok, {{one_for_one, 1, 5}, Procs}}.
