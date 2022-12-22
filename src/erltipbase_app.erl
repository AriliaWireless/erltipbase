-module(erltipbase_app).
-behaviour(application).


-export([start/2, start/0]).
-export([stop/1]).

start(_Type, _Args) ->
	erltipbase_sup:start_link().

start() ->
	application:start(inets),
	application:ensure_all_started(erltipbase),
	microservice_kafka_handler:start(openwifi),
	erltipbase_sup:start_link().

stop(_State) ->
	ok.
