-module(scanned_file).
-export([of_filename/1, of_int/1, get_name/1, get_number/1, get_raw_name/1, get_createdate/1]).

-define(USE_STRICT_REGEX, true).
-include_lib("kernel/include/file.hrl").


%type t =
% { name         : binary
% ; raw_filename : string
% ; number       : int
% ; create_date  : string
% }

new(Filename, Raw_filename, Number, Create_date) ->
	{list_to_binary(Filename), Raw_filename, Number, Create_date}.


of_filename(Filename) when is_binary(Filename) ->
	of_filename(binary_to_list(Filename));
of_filename(Filename) when is_list(Filename) ->
	case num_of_file(Filename) of
		{ok, Int} ->
			%Doing this regeneration may have an issue in that scan-000001.png => scan-1.png,
			% if this is on real files it may cause the filenames to not match up as raw strings
			create(Int, false);
		{error, _}=E -> E
	end.

of_int(Int) ->
	create(Int, true).

create(Int, IsNew) ->
	Filename = io_lib:format("scan-~w.png",[Int]),
	RawFilename = io_lib:format("scan-~w.tiff",[Int]),
  Createdate = case IsNew of
    true ->
      datetime_to_string(calendary:local_time());
    false ->
      fetch_file_createdate(Filename)
  end,
	{ok, new(Filename, RawFilename, Int, Createdate)}.

get_name({Filename, _, _,_}) -> Filename.
get_raw_name({_, RawFilename, _ , _}) -> RawFilename.
get_number({_, _, Number, _}) -> Number.
get_createdate({_,_,_,Create_date})->Create_date.

num_of_file(Filename) ->
	Strict_regex = "scan-([1-9][0-9]*)\.png",
	Lax_regex = "scan-([0-9]+)\.png",
	Regex = case ?USE_STRICT_REGEX of
		true -> Strict_regex;
		false -> Lax_regex
	end,
	case re:run(Filename, Regex, [{capture, all_but_first, list}]) of
		nomatch -> {error, "invalid filename"};
		{match, Capture} ->
			case Capture of
				[Num] ->
					case string:to_integer(Num) of
						{Int, []} -> {ok, Int};
						_ -> {error, "Did not get clean integer parse"}
					end;
				_ -> {error, "Valid filename, though didnt get one match as expected"}
			end
	end.

fetch_file_createdate(Filename) ->
	Scan_dir = config_server:query_config(scan_dir),
  Filepath = filename:join(Scan_dir,Filename),
  case file:read_file_info(Filepath) of
    {ok, Info} ->
      Mdate = Info#file_info.mtime,
      datetime_to_string(Mdate);
    {error, _Reason} ->
      "Unknown"
  end.

datetime_to_string({{Y, Mo, D},{H, Mi, S}}) ->
  io_lib:format("~4..0B/~2..0B/~2..0B - ~2..0B:~2..0B:~2..0B",[Y, Mo, D, H, Mi, S]).


