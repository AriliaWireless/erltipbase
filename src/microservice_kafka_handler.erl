-module(microservice_kafka_handler).
%% needed for the #kafka_message record definition
-include("deps/brod/include/brod.hrl").

-behaviour(brod_topic_subscriber).

-export([start/1]).
%% callback api
-export([init/2, handle_message/4, handle_message/3, send_message/3, creation_info/0]).

-record(state, {
	  offset_dir   :: string(),
		message_type :: message | message_type
}).

creation_info() ->
	[	#{	id => ?MODULE ,
	       start => { ?MODULE , start, [openwifi] },
	       restart => permanent,
	       shutdown => 100,
	       type => worker,
	       modules => [?MODULE]} ].

send_message(Topic,Key,Payload) ->
	brod:produce_sync(_Client    = ?MODULE,
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
	{ok,Clients} = application:get_env(brod,clients),
	OWConfig = proplists:get_value(openwifi, Clients),
	BootstrapHosts = proplists:get_value(endpoints, OWConfig),
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

