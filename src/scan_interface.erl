-module(scan_interface).

-export([start_scan/2, start_scan/3]).

-define(FILE_EXT, ".png").
-define(TEST_LOG_PATH, "/home/jonty/scanweb/testlog.txt").

-define(DEFAULT_RESOLUTION, "300").
-define(VALID_RESOLUTIONS, ["75","100","200","300","600","1200"]).
-define(DEFAULT_MODE, "Color").
-define(VALID_MODES, ["Lineart", "Gray", "Color"]).





start_scan(Pid, Opts) ->
    start_scan(Pid, Opts, false).
start_scan(Pid, Opts, IsTest) ->
    io:format("Spawning scan proc~n"),


    io:format("start_scan got Opts: ~p~n",[Opts]),
    %set up arg options
    Resolution = case lists:keyfind(resolution, 1, Opts) of
        {resolution, BinaryR} -> 
            R = binary_to_list(BinaryR),
            ResIsMember = lists:member(R, ?VALID_RESOLUTIONS),
            io:format("Resolution ~s is valid: ~p~n", [R, ResIsMember]),
            if ResIsMember ->
                R;
            true ->
                ?DEFAULT_RESOLUTION
            end;
        false -> ?DEFAULT_RESOLUTION
    end,
    Mode = case lists:keyfind(mode, 1, Opts) of
        {mode, BinaryM} -> 
            M = binary_to_list(BinaryM),
            ModeIsMember = lists:member(M, ?VALID_MODES),
            io:format("Mode ~s is valid: ~p~n", [M, ModeIsMember]),
            if ModeIsMember ->
                M;
            true ->
                ?DEFAULT_MODE
            end;
        false -> ?DEFAULT_MODE
    end,



    ScanPid = spawn(fun() -> do_scan(Pid, Resolution, Mode, IsTest) end),
    {ScanPid, {Resolution, Mode}}.

% do_scan(Pid, Resolution, Mode) ->
%     do_scan(Pid, Resolution, Mode, false).
    
do_scan(Pid, Resolution, Mode, IsTest) ->
    NewFile = scanned_file_server:new_file(),
    %Num = erlang:unique_integer([positive]),
    %InitFilename = io_lib:format("scan-~w.tiff", [Num]),
    InitFilename = scanned_file:get_raw_name(NewFile),
    InitFilepath = io_lib:format("~~/scanweb/priv/scans/~s", [InitFilename]),
    %ConvertFilename = io_lib:format("scan-~w~s", [Num, ?FILE_EXT]),
    ConvertFilename = scanned_file:get_name(NewFile),
    ConvertFilepath = io_lib:format("~~/scanweb/priv/scans/~s", [ConvertFilename]),
    io:format("Filename: ~s~n", [InitFilename]),


    Cmd = case IsTest of
        true ->
            io_lib:format(
                "date >> ~s; echo 'scanimage --resolution ~s --mode ~s --format tiff > ~s' >> ~s; sleep 5",
                [
                    ?TEST_LOG_PATH,
                    Resolution,
                    Mode,
                    InitFilepath,
                    ?TEST_LOG_PATH
                ]);
        false ->
            io_lib:format("scanimage --resolution ~s --mode ~s --format tiff > ~s",
                [
                    Resolution,
                    Mode,
                    InitFilepath
                ]
            )
    end,
    io:format("Command: ~s~n", [Cmd]),
    Output = os:cmd(Cmd),
    ReturnVal = os:cmd("echo $?"),
    io:format("Scan Return Val: ~p~n", [ReturnVal]),
    case ReturnVal of
        "0\n" when IsTest=:=false->
            %convert then send it off
            convert_file(Pid, InitFilepath, ConvertFilename, ConvertFilepath, NewFile);
        "0\n" when IsTest=:=true ->        
            scanned_file_server:confirm_new_file(NewFile),
            Pid ! {scan, success, "test"};
        "1\n" ->
            %fail
            scanned_file_server:fail_new_file(NewFile),
            Pid ! {scan, fail, Output}        
    end.



convert_file(Pid, InitFilepath, ConvertFilename, ConvertFilepath, ScannedFile) ->
    Cmd = io_lib:format("convert ~s ~s", [InitFilepath, ConvertFilepath]),
    io:format("Converting File. Cmd: \"~s\"~n",[Cmd]),
    Output = os:cmd(Cmd),
    ReturnVal = os:cmd("echo $?"),
    io:format("Convert Return Val: ~p~n", [ReturnVal]),
    case ReturnVal of
        "0\n" ->
            scanned_file_server:confirm_new_file(ScannedFile),
            Pid ! {scan, success, ConvertFilename};
        "1\n" ->
            scanned_file_server:fail_new_file(ScannedFile),
            Pid ! {scan, fail, Output}
    end.