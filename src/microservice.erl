-module(microservice).
-behaviour(gen_server).


%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).
-export([creation_info/0]).

-record(state, {
		advertised = false ::boolean(),
		kafka_timer :: timer:tref(),
		app_name = <<>> ::binary(),
		id = 0 :: integer(),
		public_end_point = <<>> ::binary(),
		private_end_point = <<>> ::binary(),
		hash = <<>> ::binary(),
		version = <<>> ::binary(),
		data_dir :: binary()
}).

%% API.
creation_info() ->
	[	#{	id => ?MODULE ,
	       start => { ?MODULE , start_link, [] },
	       restart => permanent,
	       shutdown => 100,
	       type => worker,
	       modules => [?MODULE]} ].

%% Obj.set(KafkaTopics::ServiceEvents::Fields::EVENT,Type);
%%	    Obj.set(KafkaTopics::ServiceEvents::Fields::ID,ID_);
%%	    Obj.set(KafkaTopics::ServiceEvents::Fields::TYPE,Poco::toLower(DAEMON_APP_NAME));
%%	    Obj.set(KafkaTopics::ServiceEvents::Fields::PUBLIC,MyPublicEndPoint_);
%%	    Obj.set(KafkaTopics::ServiceEvents::Fields::PRIVATE,MyPrivateEndPoint_);
%%	    Obj.set(KafkaTopics::ServiceEvents::Fields::KEY,MyHash_);
%%	    Obj.set(KafkaTopics::ServiceEvents::Fields::VRSN,Version_);

finish_system_message(Msg, State) when is_map(Msg) ->
	Msg#{<<"id">> => State#state.id,
			<<"type">> => State#state.app_name,
			<<"publicEndPoint">> => State#state.public_end_point,
			<<"privateEndPoint">> => State#state.private_end_point,
			<<"key">> => State#state.hash,
			<<"version">> => State#state.version}.

make_system_message(join, State) ->
	finish_system_message(#{
			<<"event">> => <<"join">>}, State);

make_system_message(keep_alive, State) ->
	finish_system_message(#{
      <<"event">> => <<"keep-alive">>}, State);
make_system_message(leaave, State) ->
	finish_system_message(#{
      <<"event">> => <<"leave">>}, State).

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link(?MODULE, [], []).

%% gen_server.

init([]) ->
	{ok, InternalEndPoint } = application:get_env(restapi_internal_uri),
	{ok, PublicEndPoint } = application:get_env(restapi_external_uri),
	{ok, KafkaTimer } = timer:send_interval(10000, system_event_ping ),
	{ok, DataDir } = application:get_env(data_dir),
	{ok, #state{ kafka_timer = KafkaTimer,
		private_end_point = InternalEndPoint,
		public_end_point = PublicEndPoint,
		data_dir = DataDir }}.

handle_call(_Request, _From, State) ->
	io:format("handle_call"),
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	io:format("handle_call: ~p", _Msg),
	{noreply, State}.

handle_info(system_event_ping, State) ->
	io:format("system_event_ping: ~p",[application:get_env(restapi_internal_port)]),
	_Msg = case State#state.advertised of
					false -> make_system_message(join, State);
					true -> make_system_message(keep_alive, State)
	      end,

	{noreply, State#state{ advertised = true} };

handle_info(_Info, State) ->
	io:format("handle_info"),
	{noreply, State}.

terminate(_Reason, State) ->
	timer:cancel(State#state.kafka_timer),
	io:format("terminate"),
	_Msg = make_system_message(leave,State),
	ok.

code_change(_OldVsn, State, _Extra) ->
	io:format("handlcode_changee_call"),
	{ok, State}.
