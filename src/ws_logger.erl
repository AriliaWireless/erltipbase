%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2023, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 05. Feb 2023 9:21 p.m.
%%%-------------------------------------------------------------------
-module(ws_logger).
-author("stephb").

-export([adding_handler/1, removing_handler/1, log/2]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

adding_handler(Config) ->
	MyConfig = maps:get(config,Config,#{file => "myhandler2.log"}),
	{ok, Pid} = gen_server:start(?MODULE, MyConfig, []),
	{ok, Config#{config => MyConfig#{pid => Pid}}}.

removing_handler(#{config := #{pid := Pid}}) ->
	gen_server:stop(Pid).

log(LogEvent,#{config := #{pid := Pid}} = Config) ->
	gen_server:cast(Pid, {log, LogEvent, Config}).

init(#{file := File}) ->
	{ok, Fd} = file:open(File, [append, {encoding, utf8}]),
	{ok, #{file => File, fd => Fd}}.

handle_call(_, _, State) ->
	{reply, {error, bad_request}, State}.

handle_cast({log, LogEvent, Config}, #{fd := Fd} = State) ->
	do_log(Fd, LogEvent, Config),
	{noreply, State}.

terminate(_Reason, #{fd := Fd}) ->
	_ = file:close(Fd),
	ok.

do_log(_Fd, LogEvent, #{formatter := {FModule, FConfig}}) ->
	String = FModule:format(LogEvent, FConfig),
	%% io:format("wslogger do_log: ~p~n",[String]),
	ws_user_registry:send_frame_to_all(binary:list_to_bin(String)).
