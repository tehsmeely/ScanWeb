-module(scan_server).
%% gen_server_mini_template
-behaviour(gen_server).
-export([start_link/0, get_scan/1, get_is_active/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(BLANKSTATE, #{
		active => false,
		scanPid => none,
		pendingRequests => [],
		scanInfo => #{}
	}).

start_link() -> 
	io:format("Scan server starting~n"),
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
init([]) ->
	io:format("Scan server initiating~n"),
	{ok, ?BLANKSTATE}.


get_scan(Opts) ->
	gen_server:call(?SERVER, {get_scan, Opts}).

get_is_active() ->
	gen_server:call(?SERVER, get_is_active).


handle_call(get_is_active, _From, State) ->
	Reply = case maps:get(active, State) of
		true -> {true, maps:get(scanInfo, State)};
		false -> false
	end,
	{reply, Reply, State};
handle_call({get_scan, Opts}, {ReqPid, _}, State) -> 
	NewState = case maps:get(active, State) of
		false ->
			{NewScanPid, {FinalResolution, FinalMode}} = scan_interface:start_scan(self(), Opts),
			{ok, StartTime} = tempo:format_now(<<"%k:%M:%S UTC %a %e %b %Y">>, erlang:timestamp()),
			% e.g. <<"21:40:05 UTC Sun 14 May 2017">> (n.b. is binary)
			State#{
				active := true,
				scanPid := NewScanPid,
				pendingRequests := [ReqPid],
				scanInfo := #{
					<<"STARTED">> => StartTime,
					<<"MODE">> => list_to_binary(FinalMode),
					<<"DPI">> => list_to_binary(FinalResolution)
				}
			};
		true ->
			OldPendingReqs = maps:get(pendingRequests, State),
			State#{
				pendingRequests := [ReqPid|OldPendingReqs]
			}
	end,
	{reply, maps:get(scanInfo, NewState), NewState};
handle_call(_Request, _From, State) -> {reply, aReply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info({scan, _, _}=M, State) ->
	io:format("Scan server got ~p. Sending on to waiting participants~n",[M]),
	lists:foreach(
		fun(Pid) ->
			Pid ! M
		end,
		maps:get(pendingRequests, State)
	),
	{noreply, ?BLANKSTATE};


handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

