-module(scanned_file).
-export([of_filename/1, of_int/1, get_name/1, get_number/1, get_raw_name/1]).

new(Filename, Raw_filename, Number) ->
	{list_to_binary(Filename), Raw_filename, Number}.


of_filename(Filename) when is_binary(Filename) ->
	of_filename(binary_to_list(Filename));
of_filename(Filename) when is_list(Filename) ->
	case num_of_file(Filename) of
		{ok, Int} -> 
			%Doing this regeneration may have an issue in that scan-000001.png => scan-1.png,
			% if this is on real files it may cause the filenames to not match up as raw strings
			of_int(Int);
		{error, _}=E -> E 
	end.

of_int(Int) ->
	Filename = io_lib:format("scan-~w.png",[Int]),
	RawFilename = io_lib:format("scan-~w.tiff",[Int]),
	{ok, new(Filename, RawFilename, Int)}.

get_name({Filename, _, _}) -> Filename.
get_raw_name({_, RawFilename, _}) -> RawFilename.
get_number({_, _, Number}) -> Number.

num_of_file(Filename) ->
	case re:run(Filename, "scan-([0-9]+)\.png", [{capture, all_but_first, list}]) of
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
