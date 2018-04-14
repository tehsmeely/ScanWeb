-module(static_file_manager).

-export([compile_and_render/0]).


% This manages the static files generated for the project at runtime/startup


compile_and_render() ->
	compile_file("static/html/index.html.templt"),
	compile_file("static/js/index.js.templt"),
	compile_file("static/css/index.css.templt").





compile_file(InFile) ->
	OutFile = filename:rootname(InFile, ".templt"),
	Compiled = mustache:compile(static_file_config, priv_file(InFile)),
	write_to_priv(
		OutFile,
		mustache:render(static_file_config, Compiled)
	).


priv_file(Filename) ->
    Priv_dir = case code:priv_dir(scanweb) of
        {error, bad_name} ->
            "priv";
        PrivDir ->
            PrivDir
    end,
    filename:join([Priv_dir, Filename]).

write_to_priv(Filename, Body) ->
	Priv_file = priv_file(Filename),
	case file:write_file(Priv_file, Body) of
		ok -> 
			io:format("Success writing file ~s ~n",[Priv_file]);	
		{error, _Reason} ->
			io:format("Failed to write file ~s ~n",[Priv_file])
	end.