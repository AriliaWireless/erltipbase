-module(microservice_kafka_handler).
-behaviour(brod_topic_subscriber).

-include_lib("brod/include/brod.hrl"). %% needed for the #kafka_message record definition

-export([start/1]).
-export([init/2, handle_message/4, handle_message/3]). %% callback api

-record(state, { offset_dir   :: string()
	, message_type :: message | message_type
}).

init(_Topic, MessageType) ->
	State = #state{
		message_type =  MessageType
	},
	{ok, [], State}.

-spec start(brod:client_id()) -> {ok, pid()}.
start(ClientId) ->
	Topic = <<"service_events">>,
	GroupConfig = [{offset_commit_policy, commit_to_kafka_v2},
	               {offset_commit_interval_seconds, 5}
	],
	GroupId = <<"my-unique-group-id-shared-by-all-members">>,
	ConsumerConfig = [{begin_offset, earliest}],
	brod:start_link_group_subscriber(ClientId, GroupId, [Topic],
	                                 GroupConfig, ConsumerConfig,
	                                 _CallbackModule  = ?MODULE,
	                                 _CallbackInitArg = []).
%%	Config = [{offset_reset_policy, reset_to_earliest}],
%%	brod_topic_subscriber:start_link(ClientId, Topic, all,
%%	                                 Config, message,
%%	                                 ?MODULE,
%	                                 []).

handle_message(_Partition, Message, State) when is_record(Message,kafka_message) ->
	Msg = jsone:decode(Message#kafka_message.value),
	io:format("Message: ~p~n",[Msg]),
	case maps:get(<<"event">>,Msg,<<"">>) of
		<<"join">> ->
			microservice:add_service_info(Msg);
		<<"keep-alive">> ->
			microservice:add_service_info(Msg);
		<<"leave">> ->
			microservice:delete_service_info(Msg);
		_ ->
			io:format("")
	end,
	{ok, ack, State}.

handle_message(_Topic, Partition, Message, State) ->
	#kafka_message{ offset = Offset
		, key   = Key
		, value = Value
	} = Message,
	io:format("~p ~p: offset:~w key:~s value:~s\n",
	                      [self(), Partition, Offset, Key, Value]),
	{ok, ack, State}.

