%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. May 2020 4:26 p.m.
%%%-------------------------------------------------------------------
-module(restlib).
-author("stephb").

%% API
-export([get_access_token/1,get_caller_id/1]).

get_access_token(_Req)->
	{ok,token}.

%get_access_token(Req) ->
%	case cowboy_req:header(<<"authorization">>, Req) of
%		<<"Bearer ", Token/binary>> ->
%			{ok, Token};
%		_ ->
%			QsVals = cowboy_req:parse_qs(Req),
%			case proplists:get_value(<<"access_token">>, QsVals,undefined) of
%				undefined ->
%					{error, missing};
%				Token ->
%					{ ok , Token }
%			end
%	end.

get_caller_id(_Token)->
	{ ok , default }.