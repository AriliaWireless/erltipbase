%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2023, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 05. Feb 2023 1:33 p.m.
%%%-------------------------------------------------------------------
-module(tests).
-author("stephb").

%% API
-export([ws1/0, log1/0]).

ws1() ->
	ws_user_registry:send_frame(<<"script.runner@arilia.com">>,{text,<<"Hello">>}).

log1() ->
	logger:info("Test logging").