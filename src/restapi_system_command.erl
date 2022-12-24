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

-record(state, {method, token, session_time, caller_id}).

-type request_data() :: #{}.
-type request_state() :: #state{}.
-type request_answer() :: {boolean()|binary()|list()|tuple()|undefined|ok|stop, request_data(), request_state()}.

%% API
-export([init/2, terminate/3, allowed_methods/2, allow_missing_post/2, charsets_provided/2, content_types_accepted/2,
         content_types_provided/2, is_conflict/2, valid_content_headers/2, delete_completed/2, delete_resource/2, expires/2,
         previously_existed/2, resource_exists/2, is_authorized/2, forbidden/2, generate_etag/2, known_methods/2, languages_provided/2,
         last_modified/2, malformed_request/2, moved_permanently/2, moved_temporarily/2, options/2, multiple_choices/2, rate_limited/2,
         service_available/2, uri_too_long/2, valid_entity_length/2, variances/2,
         from_json/2, to_json/2]).


-spec init(Req :: request_data(), any()) -> request_answer().
init(Req, _State) ->
	io:format("REST Called! ~p~n~n",[Req]),
	{cowboy_rest, Req, #state{session_time = os:system_time(), method = cowboy_req:method(Req)}}.

-spec terminate(Reason :: any(), Req :: request_data(), any()) -> ok.
terminate(_Reason, _Req, _State) ->
	ok.

-spec allowed_methods(Req :: request_data(), State :: request_state()) -> request_answer().
allowed_methods(Req, State) ->
	{?_RESOURCE_METHODS, Req, State}.

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
from_json(Req, #state{method = <<"GET">>} = _State) ->
	io:format("GET called~n"),
	cowboy_req:reply(400, #{}, <<"Missing echo parameter.">>, Req);
from_json(Req, #state{method = <<"POST">>} = State) ->
	io:format("POST called~n"),
	{ok, Req, State};
from_json(Req, #state{method = <<"PUT">>} = State) ->
	io:format("PUT called~n"),
	{ok, Req, State};
from_json(Req, #state{method = <<"HEAD">>} = State) ->
	io:format("HEAD called~n"),
	{ok, Req, State};
from_json(Req, #state{method = <<"PATCH">>} = State) ->
	io:format("PATCH called~n"),
	{ok, Req, State};
from_json(Req, #state{method = <<"OPTIONS">>} = State) ->
	io:format("OPTIONS called~n"),
	{ok, Req, State};
from_json(Req, #state{method = <<"DELETE">>} = State) ->
	io:format("DELETE called~n"),
	{ok, Req, State};
from_json(Req, State) ->
	io:format("Nothing we know. called~n"),
	{ok, Req, State}.

-spec to_json(Req :: request_data(), State :: request_state()) -> request_answer().
to_json(Req, State) ->
	{ok, Req, State}.

-spec delete_completed(Req :: request_data(), State :: request_state()) -> request_answer().
delete_completed(Req, State) ->
	{true, Req, State}.

-spec delete_resource(Req :: request_data(), State :: request_state()) -> request_answer().
delete_resource(Req, State) ->
	{false, Req, State}.

-spec expires(Req :: request_data(), State :: request_state()) -> request_answer().
expires(Req, State) ->
	{undefined, Req, State}.

-spec forbidden(Req :: request_data(), State :: request_state()) -> request_answer().
forbidden(Req, State) ->
	{false, Req, State}.

-spec generate_etag(Req :: request_data(), State :: request_state()) -> request_answer().
generate_etag(Req, State) ->
	{undefined, Req, State}.

-spec is_authorized(Req :: request_data(), State :: request_state()) -> request_answer().
is_authorized(Req, State) ->
	case restlib:get_access_token(Req) of
		{ok, Token} ->
			case restlib:get_caller_id(Token) of
				{ok, CallerId} ->
					{true, Req, State#state{token = Token, caller_id = CallerId}};
				{error, _Reason} ->
					{{false, <<"Bearer">>}, Req, State}
			end;
		{error, _Reason} ->
			{{false, <<"Bearer">>}, Req, State}
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

-spec moved_permanently(Req :: request_data(), State :: request_state()) -> request_answer().
moved_permanently(Req, State) ->
	{false, Req, State}.

-spec moved_temporarily(Req :: request_data(), State :: request_state()) -> request_answer().
moved_temporarily(Req, State) ->
	{false, Req, State}.

-spec multiple_choices(Req :: request_data(), State :: request_state()) -> request_answer().
multiple_choices(Req, State) ->
	{false, Req, State}.

-spec options(Req :: request_data(), State :: request_state()) -> request_answer().
options(Req, State) ->
	{ok, Req, State}.

-spec previously_existed(Req :: request_data(), State :: request_state()) -> request_answer().
previously_existed(Req, State) ->
	{false, Req, State}.

-spec rate_limited(Req :: request_data(), State :: request_state()) -> request_answer().
rate_limited(Req, State) ->
	{false, Req, State}.

-spec resource_exists(Req :: request_data(), State :: request_state()) -> request_answer().
resource_exists(Req, State) ->
	{true, Req, State}.

-spec service_available(Req :: request_data(), State :: request_state()) -> request_answer().
service_available(Req, State) ->
	{true, Req, State}.

-spec uri_too_long(Req :: request_data(), State :: request_state()) -> request_answer().
uri_too_long(Req, State) ->
	{false, Req, State}.

-spec valid_content_headers(Req :: request_data(), State :: request_state()) -> request_answer().
valid_content_headers(Req, State) ->
	{true, Req, State}.

-spec valid_entity_length(Req :: request_data(), State :: request_state()) -> request_answer().
valid_entity_length(Req, State) ->
	{true, Req, State}.

-spec variances(Req :: request_data(), State :: request_state()) -> request_answer().
variances(Req, State) ->
	{[], Req, State}.