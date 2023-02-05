%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2023, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 03. Feb 2023 10:34 p.m.
%%%-------------------------------------------------------------------
-module(restapi_ws_handler).
-author("stephb").

%% API
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).

-record(ws_state, {
	authenticated = false :: boolean(),
	email = <<>> :: binary(),
	userinfo
}).

init(Req,_State) ->
	io:format("WS Init~n"),
	{cowboy_websocket, Req, #ws_state{} }.

websocket_init(State) ->
	io:format("WS Init 2~n"),
	{ok, State}.

websocket_handle( {text, <<"Token:", Token/binary>>} , #ws_state{ authenticated = false}=State ) ->
	case security_sdk:validate_token(Token) of
		{error, _ErrorCode} ->
			io:format("WS: Token is not valid: ~p~n", [Token]),
			{stop, State};
		{ok, EMail,Userinfo} ->
			io:format("WS: Token is valid: ~p~n", [Token]),
			{ok, State#ws_state{ email = EMail, userinfo = Userinfo}}
	end;

websocket_handle( {text, Data} , State ) ->
	io:format("Data: ~p~n", [Data]),
	{ok, State};
websocket_handle( {binary, Data} , State ) ->
	io:format("Data: ~p~n", [ Data]),
	{ok, State}.

websocket_info({log, Text}, State) ->
	io:format("Data: ~p~n", [Text]),
	{[{text, Text}], State};
websocket_info(Info, State) ->
	io:format("Data: ~p~n", [Info]),
	{ok, State}.