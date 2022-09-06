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
	kafka_timer :: timer:tref()
}).

%% API.
creation_info() ->
	[	#{	id => ?MODULE ,
	       start => { ?MODULE , start_link, [] },
	       restart => permanent,
	       shutdown => 100,
	       type => worker,
	       modules => [?MODULE]} ].

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link(?MODULE, [], []).

%% gen_server.

init([]) ->
	{ok, KafkaTimer } = timer:send_interval(10000, system_event_ping ),
	{ok, #state{ kafka_timer = KafkaTimer}}.

handle_call(_Request, _From, State) ->
	io:format("handle_call"),
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	io:format("handle_call: ~p", _Msg),
	{noreply, State}.

handle_info(system_event_ping, State) ->
	io:format("system_event_ping"),
	{noreply, State};

handle_info(_Info, State) ->
	io:format("handle_info"),
	{noreply, State}.

terminate(_Reason, State) ->
	io:format("terminate"),
	timer:cancel(State#state.kafka_timer),
	ok.

code_change(_OldVsn, State, _Extra) ->
	io:format("handlcode_changee_call"),
	{ok, State}.
