-module(erltipbase_app).
-behaviour(application).


-export([start/2, start/0]).
-export([stop/1]).

start(_Type, _Args) ->
	erltipbase_sup:start_link().

start() ->
	io:format("hello"),
	application:ensure_all_started(erltipbase),
	erltipbase_sup:start_link().

stop(_State) ->
	ok.
