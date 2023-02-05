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
-export([authorization_verification/1,get_caller_id/1,bad_request/2,add_cors/2]).

-spec authorization_verification(cowboy_req:req()) -> {ok, string(), #{}} | {error, integer()}.
authorization_verification(Req)->
	case cowboy_req:parse_header(<<"authorization">>, Req) of
		{bearer,Token} ->
			case security_sdk:validate_token(Token) of
				{ok , EMail, Userinfo} ->
					{ ok, EMail, Userinfo};
				Error ->
					Error
			end;
		_ ->
			{ error , 500 }
	end.

get_caller_id(_Token)->
	{ ok , default }.

error_description( {ErrorCode, ErrorDetails, ErrorDescription} ) ->
	#{
		'ErrorCode' => ErrorCode,
		'ErrorDetails' => ErrorDetails,
		'ErrorDescription' => ErrorDescription
	}.

-spec bad_request( cowboy_req:req(), { integer(), string(), string()}) ->
	{ integer(), cowboy_req:req()}.
bad_request(Req, {ErrorCode, ErrorDetails, ErrorDescription} = ErrorInformation) when is_integer(ErrorCode), is_list(ErrorDetails), is_list(ErrorDescription) ->
	{ 400, cowboy_req:set_resp_body(jsone:encode(error_description(ErrorInformation)), Req) }.

add_cors(Req0, Methods) ->
	Origin = cowboy_req:header(<<"origin">>, Req0,<<"*">>),
	Req1 = cowboy_req:set_resp_header(<<"access-control-allow-credentials">>, <<"true">>, Req0),
%%	Req2 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"authorization, content-type, X-PINGOTHER">>, Req1),
	Req2 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"authorization, content-type">>, Req1),
	Req3 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, Origin, Req2),
	Req4 = cowboy_req:set_resp_header(<<"vary">>, <<"origin, accept-encoding">>, Req3),
	Req5 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, Methods, Req4),
	Req6 = cowboy_req:set_resp_header(<<"accept">>, <<"*/*">>, Req5),
	cowboy_req:set_resp_header(<<"access-control-max-age">>, <<"20">>, Req6).
