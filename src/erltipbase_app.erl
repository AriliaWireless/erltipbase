-module(erltipbase_app).
-behaviour(application).


-export([start/2, start/0]).
-export([stop/1]).

start(_Type, _Args) ->
	utils:set_app_name(erltipbase),
	erltipbase_sup:start_link().

start() ->
	utils:set_app_name(erltipbase),
	application:start(inets),
	application:ensure_all_started(utils:get_app_name()),
	microservice_kafka_handler:start(openwifi),
	erltipbase_sup:start_link().

stop(_State) ->
	ok.
