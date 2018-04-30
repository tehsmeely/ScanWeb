-module(scanned_file_server).
-behaviour(gen_server).
%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-define(SERVER, ?MODULE).

-export([get_all/0, get_latest/0, get_snapshot/0, new_file/0, refresh_files/0,
	confirm_new_file/1, fail_new_file/1]).
%Name of config file, path starting at priv/ dir
-define(DEFAULT_SCAN_DIR, "/var/scans/").
-define(ELSE, true).

-record(state, {files, pending_files}).

start_link() ->
	io:format("scanned_file_server start~n"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	io:format("scanned_file_server init, pid~p~n",[self()]),
	self() ! init,
    {ok, #state{files=[], pending_files=[]}}.




%% API

%CALLS
get_all() ->
	gen_server:call(?SERVER, {get_files, all}).
get_latest() ->
	gen_server:call(?SERVER, {get_files, latest}).
get_snapshot() ->
	gen_server:call(?SERVER, {get_files, snap}).
new_file() ->
	gen_server:call(?SERVER, new_file).

confirm_new_file(Scanned_file) ->
	gen_server:call(?SERVER, {update_pending, confirm, Scanned_file}).

fail_new_file(Scanned_file) ->
	gen_server:call(?SERVER, {update_pending, fail, Scanned_file}).

%CASTS
refresh_files() ->
	gen_server:cast(?SERVER, refresh_files).

%% Internal functions

read_files_direct () ->
	Scan_dir = config_server:query_config(scan_dir, ?DEFAULT_SCAN_DIR),
	{ok, Filenames} = file:list_dir(Scan_dir),
	% Soon: It rightly should die if the scandir is invalid, but a badmatch
	% probably isnt the right way to do it - this should propagate,
	% or get checked earlier
	Files = lists:filtermap(
		fun(Filename) -> 
			IsPng = string:right(Filename, 4) =:= ".png",
			if IsPng ->
				case scanned_file:of_filename(Filename) of
					{ok, Scanned_file}=G ->
						io:format("made scanned file: ~p~n",[G]),
						{true, Scanned_file};
					{error, Reason} ->
						io:format("Failed to convert file to scanned_file. ~p~n", [Reason]),
						false
				end;
			true ->
				false
			end
		end,
		Filenames
	),
	lists:reverse(
		lists:sort(
			fun(F1, F2) -> scanned_file:get_number(F1) < scanned_file:get_number(F2) end
		,
			Files
		)
	).

convert_to_name_only(FileList) ->
	lists:map(
		fun(Scanned_file) ->
			scanned_file:get_name(Scanned_file)
		end
	,
		FileList
	).
convert_to_file_and_description(FileList) ->
	lists:map(
		fun(Scanned_file) ->
			[ scanned_file:get_name(Scanned_file)
			,	list_to_binary(io_lib:format("~s (~s)", [
					scanned_file:get_name(Scanned_file),
					scanned_file:get_createdate(Scanned_file)
				]))
			]
		end
	,
		FileList
	).
highest_num_of_list(List) ->
	highest_num_of_list(List, 0).
highest_num_of_list([], Highest)->
	Highest;
highest_num_of_list([Next|Tl], Highest)->
	N = scanned_file:get_number(Next),
	M = if 
		N > Highest -> N;
		?ELSE -> Highest
	end,
	highest_num_of_list(Tl, M).

filter_pending_files(All_files, Pending_files) ->
	%If any files from All_files are in pending, remove them
	lists:filter(
		fun(Pending_file) ->
			not lists:member(Pending_file, All_files)
		end
	,
		Pending_files
	).

%% Gen_server internal callbacks 

handle_call({get_files, GetWhich}, _From, State) ->
	Files = convert_to_name_only(State#state.files),

	Reply = case GetWhich of
		all -> Files;
		latest ->
			case Files of
				[] -> {error, no_files};
				[Latest |_] -> {ok, Latest}
			end;
		snap ->
			case Files of
				[] -> 
					{error, no_files};
				[Latest | _] -> 
					{ok, {Latest, convert_to_file_and_description(State#state.files)}}
			end
	end,
	{reply, Reply, State};
handle_call(new_file, _From, State) ->
	ConfirmedFiles = State#state.files,
	PendingFiles = State#state.pending_files,
	Highest_confirmed = case ConfirmedFiles of
	 [] -> 1;
	 [Latest|_] -> scanned_file:get_number(Latest) + 1
	end,
	Highest_pending = case PendingFiles of
	 [] -> 1;
	 More -> highest_num_of_list(More) + 1
	end,
	Next_num = max(Highest_confirmed, Highest_pending),
	NewFile = scanned_file:of_int(Next_num),
	NewState = State#state{pending_files=[NewFile|PendingFiles]},
	{reply, NewFile, NewState};
handle_call({update_pending, Status, Scanned_file}, _From, State) ->
	ConfirmedFiles = State#state.files,
	NewPendingFiles = lists:delete(Scanned_file, State#state.pending_files),
	NewConfirmedFiles = case Status of
		confirm ->
			[Scanned_file|ConfirmedFiles];
		fail ->
			ConfirmedFiles
	end,
	NewState = State#state{files=NewConfirmedFiles, pending_files=NewPendingFiles},
    {reply, ignored, NewState};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.


handle_cast(refresh_files, State) ->
	All_files = read_files_direct(),
	PendingFiles = filter_pending_files(All_files, State#state.pending_files),
    {noreply, State#state{files=All_files, pending_files=PendingFiles}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(init, State) ->
	io:format("file_server post_init~n"),
	All_files = read_files_direct(),
    {noreply, State#state{files=All_files}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
{ok, State}.
