%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2022, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 22. Dec 2022 9:50 a.m.
%%%-------------------------------------------------------------------
-module(restapi_system_command).
-author("stephb").

-define(_STOP_REQUEST, {stop, Req, State}).
-define(_RESOURCE_METHODS, [<<"GET">>, <<"HEAD">>, <<"OPTIONS">>, <<"DELETE">>, <<"PUT">>, <<"PATCH">>, <<"POST">>]).

-record(call_state, {
	method = <<>> :: binary(),
	token = <<>> :: binary(),
	session_time = os:system_time(second),
	caller_id = <<>> :: binary(),
	command = <<>> :: binary(),
	email = [] :: string(),
	userinfo
	}
).

-type request_data() :: #{}.
-type request_state() :: #call_state{}.
-type request_answer() :: {boolean()|binary()|list()|tuple()|undefined|ok|stop, request_data(), request_state()}.

%% API
-export([init/2, terminate/3, allowed_methods/2, allow_missing_post/2, charsets_provided/2, content_types_accepted/2,
         content_types_provided/2, is_conflict/2, valid_content_headers/2, is_authorized/2, generate_etag/2, known_methods/2, languages_provided/2,
         last_modified/2, malformed_request/2,
         options/2,
         service_available/2, valid_entity_length/2,
         variances/2,
         from_json/2, to_json/2]).

-spec init(Req :: request_data(), any()) -> request_answer().
init(Req, _State) ->
%%	io:format("REQ-> ~p~n",[Req]),
	QS = cowboy_req:parse_qs(Req),
	NewState = #call_state{
		method = cowboy_req:method(Req),
		command = proplists:get_value(<<"command">>,QS,<<>>)
	},
	{cowboy_rest, restlib:add_cors(Req,<<"GET, POST, OPTIONS">>), NewState}.

-spec terminate(Reason :: any(), Req :: request_data(), any()) -> ok.
terminate(_Reason, _Req, _State) ->
	ok.

-spec allowed_methods(Req :: request_data(), State :: request_state()) -> request_answer().
allowed_methods(Req, State) ->
	{[<<"GET">>, <<"OPTIONS">>, <<"POST">>], Req, State}.

-spec allow_missing_post(Req :: request_data(), State :: request_state()) -> request_answer().
allow_missing_post(Req, State) ->
	{true, Req, State}.

-spec charsets_provided(Req :: request_data(), State :: request_state()) -> request_answer().
charsets_provided(Req, State) ->
	{[<<"utf-8">>], Req, State}.

-spec content_types_accepted(Req :: request_data(), State :: request_state()) -> request_answer().
content_types_accepted(Req, State) ->
	{[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.

-spec content_types_provided(Req :: request_data(), State :: request_state()) -> request_answer().
content_types_provided(Req, State) ->
	{[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

-spec from_json(Req :: request_data(), State :: request_state()) -> request_answer().
from_json(Req, #call_state{method = <<"GET">>, command= <<"info">>} = State) ->
	{ ok, Req, State};
from_json(Req, #call_state{method = <<"GET">>, command= <<>>} = State) ->
	%% io:format("to_json called 2: ~p~n",[Req]),
	{ ok, Req, State};
from_json(Req, #call_state{method = <<"POST">>} = State) ->
	%% io:format("from_json called POST: ~n"),
	{Status,NewReq} = case cowboy_req:read_body(Req) of
		{ok,Body,Req1} ->
			ParsedBody = jsone:decode(Body),
			case maps:get(<<"command">>, ParsedBody, undefined) of
				undefined ->
					restlib:bad_request(Req1, { 1001, <<"Missing command">>, <<"Command should be systemlogs, subsystems">>});
				<<"getsubsystemnames">> ->
					Answer = #{ tagList => []},
					{200,cowboy_req:set_resp_body(jsone:encode(Answer),Req)};
				<<"getloglevelnames">> ->
					Answer = #{ list => [<<"debug">>, <<"information">>]},
					{200,cowboy_req:set_resp_body(jsone:encode(Answer),Req)};
				<<"getloglevels">> ->
					Answer = #{ tagList => []},
					{200,cowboy_req:set_resp_body(jsone:encode(Answer),Req)};
				_ ->
					restlib:bad_request(Req1, { 1002, <<"Invalid command">>, <<"Invalid command">>})
			end
	end,
	EReq = cowboy_req:reply(Status,NewReq),
	{ stop, EReq, State};
from_json(Req, State) ->
	{ Status, NewReq } = restlib:bad_request(Req, { 1002, <<"Unsupported method">>, <<"Invalid method">>}),
	EReq = cowboy_req:reply(Status,NewReq),
	{ stop, EReq, State}.

-spec to_json(Req :: request_data(), State :: request_state()) -> request_answer().
to_json(Req, #call_state{method = <<"GET">>, command= <<"info">>} = State) ->
	%% io:format("to_json called GET info:~n"),
	{ok,Info} = microservice:get_my_service_info(),
	#{<<"version">> := Version} = Info,
	{ok,CertFileName} = application:get_env(utils:get_app_name(),restapi_external_cert),
	AbsCertFileName = code:priv_dir(utils:get_app_name()) + CertFileName,
	CertInfo = #{ filename => CertFileName, expiresOn => utils:certificate_expiry(AbsCertFileName) },
	Answer = #{
		version => Version,
		hostname => list_to_binary(net_adm:localhost()),
		uptime =>  State#call_state.session_time - persistent_term:get(microservice_start_time),
		start => persistent_term:get(microservice_start_time),
		processors => utils:number_of_cpus(),
		certificates => [ CertInfo ],
		os => list_to_binary(erlang:system_info(version)),
		erlangnode => node()
	},
	{jsone:encode(Answer), Req, State};
to_json(Req, State) ->
	{ Status, NewReq } = restlib:bad_request(Req, { 1002, <<"Unsupported method">>, <<"Invalid method">>}),
	EReq = cowboy_req:reply(Status,NewReq),
	{ stop, EReq, State}.

-spec generate_etag(Req :: request_data(), State :: request_state()) -> request_answer().
generate_etag(Req, State) ->
	{undefined, Req, State}.

-spec is_authorized(Req :: request_data(), State :: request_state()) -> request_answer().
is_authorized(Req, State) ->
	case State#call_state.method of
		<<"OPTIONS">> ->
			{true, Req, State};
		_ ->
			case restlib:authorization_verification(Req) of
				{ok, Email, UserInfo } ->
					%% io:format("EMail = ~p~n",[Email]),
					{true, Req, State#call_state{email = Email, userinfo = UserInfo}};
				_ ->
					{{false, <<"Bearer">>}, Req, State}
			end
	end.

-spec is_conflict(Req :: request_data(), State :: request_state()) -> request_answer().
is_conflict(Req, State) ->
	{false, Req, State}.

-spec known_methods(Req :: request_data(), State :: request_state()) -> request_answer().
known_methods(Req, State) ->
	{?_RESOURCE_METHODS, Req, State}.

-spec languages_provided(Req :: request_data(), State :: request_state()) -> request_answer().
languages_provided(Req, State) ->
	{[<<"en">>], Req, State}.

-spec last_modified(Req :: request_data(), State :: request_state()) -> request_answer().
last_modified(Req, State) ->
	{undefined, Req, State}.

-spec malformed_request(Req :: request_data(), State :: request_state()) -> request_answer().
malformed_request(Req, State) ->
	{false, Req, State}.

-spec options(Req :: request_data(), State :: request_state()) -> request_answer().
options(Req, State) ->
	{ok, Req, State}.

-spec service_available(Req :: request_data(), State :: request_state()) -> request_answer().
service_available(Req, State) ->
	{true, Req, State}.

-spec valid_content_headers(Req :: request_data(), State :: request_state()) -> request_answer().
valid_content_headers(Req, State) ->
	{true, Req, State}.

-spec valid_entity_length(Req :: request_data(), State :: request_state()) -> request_answer().
valid_entity_length(Req, State) ->
	{true, Req, State}.

-spec variances(Req :: request_data(), State :: request_state()) -> request_answer().
variances(Req, State) ->
	{[], Req, State}.