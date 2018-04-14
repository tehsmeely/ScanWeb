-module(config_server).

-behaviour(gen_server).
%% gen_server callbacks
-export([start_link/0, query_config/1, query_config/2, reload_config/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).


-define(SERVER, ?MODULE).



%Name of config file, path starting at priv/ dir
-define(CONFIG_FILE, "cfg/scanweb_conf.erl").


-record(state, {conf}).

start_link() ->
    io:format("config_server start~n"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    io:format("config_server init, pid:~p~n",[self()]),
	Internal_config = load_config(),
    {ok, #state{conf=Internal_config}}.




%% API

query_config(Key, Default) ->
    io:format("config_server:query_config Key: ~p, Default: ~p~n",[Key, Default]),
    gen_server:call(?SERVER, {query_config, Key, Default}).
query_config(Key) ->
    io:format("config_server:query_config Key: ~p~n",[Key]),
    gen_server:call(?SERVER, {query_config, Key}).

reload_config () ->
	gen_server:cast(?SERVER, reload_config).

%% Internal functions

load_config() ->
	ConfigFilename = priv_file(?CONFIG_FILE),
	case file:consult(ConfigFilename) of
		{ok, [Config]} ->
			io:format("Config loaded from ~p~n",[Config]),
			Config;
		{error, Reason} ->
			io:format("Failed to load config: ~p~n", [Reason]),
			#{}
	end.


priv_file(Filename) ->
    Priv_dir = case code:priv_dir(scanweb) of
        {error, bad_name} ->
            "priv";
        PrivDir ->
            PrivDir
    end,
    filename:join([Priv_dir, Filename]).


handle_call({query_config, Key}, _From, State) ->
    {reply, maps:get(Key, State#state.conf), State};
handle_call({query_config, Key, Default}, _From, State) ->
    {reply, maps:get(Key, State#state.conf, Default), State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(reload_config, State) ->
	NewConfig = load_config(),
    {noreply, State#state{conf=NewConfig}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(init, State) ->
    io:format(">>> config_server handling self send init msg~n"),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
{ok, State}.