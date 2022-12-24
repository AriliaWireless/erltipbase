-module(microservice_kafka_handler).
-include_lib("brod/include/brod.hrl"). %% needed for the #kafka_message record definition

-behaviour(brod_topic_subscriber).

-export([start/1]).
-export([init/2, handle_message/4, handle_message/3, send_message/3]). %% callback api

-record(state, { offset_dir   :: string()
	, message_type :: message | message_type
}).


send_message(Topic,Key,Payload) ->
	brod:produce_sync(_Client    = openwifi,
	                  _Topic     = Topic,
	                  _Partition = 0,
	                  _Key       = Key,
	                  _Value     = Payload).

init(_Topic, MessageType) ->
	State = #state{
		message_type =  MessageType
	},
	{ok, [], State}.

-spec start(brod:client_id()) -> {ok, pid()}.
start(_ClientId) ->
	ClientId = ?MODULE,
	Topic = <<"service_events">>,
	%% ConsumerConfig = [{begin_offset, earliest}],
	BootstrapHosts = [{"debfarm1-node-a",9093}],
	ok = brod:start_client(BootstrapHosts, ClientId, [{query_api_versions, false}]),
	ok = brod:start_producer(ClientId, Topic, _ProducerConfig = []),
	ConsumerConfig = [{offset_reset_policy, reset_to_earliest}],
	brod_topic_subscriber:start_link(ClientId, Topic, all,
	                                 ConsumerConfig, message,
	                                 _CallbackModule = ?MODULE,
	                                 message).

handle_message(_Partition, Message, State) when is_record(Message,kafka_message) ->
	Msg = jsone:decode(Message#kafka_message.value),
	%% io:format("Message: ~p~n",[Msg]),
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

