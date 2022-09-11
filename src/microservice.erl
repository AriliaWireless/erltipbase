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

-export([add_service_info/1,delete_service_info/1]).

-record(state, {
		advertised = false ::boolean(),
		kafka_timer :: timer:tref(),
		app_name = <<>> ::binary(),
		id = 0 :: integer(),
		public_end_point = <<>> ::binary(),
		private_end_point = <<>> ::binary(),
		hash = <<>> ::binary(),
		version = <<>> ::binary(),
		data_dir :: binary(),
		services = maps:new() ::map()
}).

%% API.
creation_info() ->
	[	#{	id => ?MODULE ,
	       start => { ?MODULE , start_link, [] },
	       restart => permanent,
	       shutdown => 100,
	       type => worker,
	       modules => [?MODULE]} ].

add_service_info(ServiceInfo) when is_map(ServiceInfo) ->
	gen_server:call(?MODULE, { register_service, ServiceInfo}).

delete_service_info(ServiceInfo) when is_map(ServiceInfo) ->
	gen_server:call(?MODULE, { remove_service, ServiceInfo}).

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
make_system_message(leave, State) ->
	finish_system_message(#{
      <<"event">> => <<"leave">>}, State).

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

%% gen_server.

init([]) ->
	{ok, InternalEndPoint } = application:get_env(restapi_internal_uri),
	{ok, PublicEndPoint } = application:get_env(restapi_external_uri),
	{ok, KafkaTimer } = timer:send_interval(10000, system_event_ping ),
	{ok, DataDir } = application:get_env(data_dir),
	{ok, Version } = application:get_env(version),
	Hash = utils:to_hex(crypto:hash(md5,integer_to_binary(registry:get(system_id)))),
	{ok, #state{ kafka_timer = KafkaTimer,
		private_end_point = InternalEndPoint,
		public_end_point = PublicEndPoint,
		data_dir = DataDir,
    app_name = <<"ow_erlhelloworld">>,
		version = Version,
		hash = Hash,
		id = registry:get(system_id)}}.

handle_call({register_service,ServiceInfo}, _From, State) ->
	case maps:find(<<"publicEndPoint">>, ServiceInfo) of
		{ok,PublicName} ->
			NewServices = maps:remove(PublicName, State#state.services),
			NewServices2 = maps:put(PublicName,ServiceInfo,NewServices),
			{reply,ok,State#state{ services = NewServices2}};
		_ ->
			{reply, ignored, State}
	end;

handle_call({remove_service,ServiceInfo}, _From, State) ->
	case maps:find(<<"publicEndPoint">>, ServiceInfo) of
		{ok,PublicName} ->
			NewServices = maps:remove(PublicName, State#state.services),
			{reply,ok,State#state{ services = NewServices}};
		_ ->
			{reply, ignored, State}
	end.

handle_cast(_Msg, State) ->
	io:format("handle_cast: ~p", _Msg),
	{noreply, State}.

handle_info(system_event_ping, State) ->
	Msg = case State#state.advertised of
					false -> make_system_message(join, State);
					true -> make_system_message(keep_alive, State)
	      end,
	io:format("Bus message: ~p~n",[Msg]),
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