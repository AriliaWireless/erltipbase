%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2023, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 05. Feb 2023 2:53 p.m.
%%%-------------------------------------------------------------------
-module(ws_process).
-author("stephb").

%% API
-export([process/2]).

-spec process(Frame::cow_ws:frame(), EMail::binary()) -> noreply | { reply , cow:ws_frame()}.
process({text,Text}, Email) ->
	io:format("Frame: ~p, ~p~n",[Text, Email]),
	{reply,{text,<<"Message received">>}};
process({binary,Text}, Email) ->
	io:format("Frame: ~p, ~p~n",[Text, Email]),
	{reply,{binary,<<"Message received">>}};
process(_Frame, _EMail) ->
	noreply.

