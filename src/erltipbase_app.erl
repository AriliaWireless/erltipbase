-module(erltipbase_app).
-behaviour(application).

-export([start/2, start/0]).
-export([stop/1]).

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		                                 {'_', [{"/api/v1", rest_server, []}]}
	                                 ]),
	{ok, _} = cowboy:start_clear(my_http_listener,
	                             [{port, 8080}],
	                             #{env => #{dispatch => Dispatch}}
	),
	erltipbase_sup:start_link().

start() ->
	io:format("hello"),
	application:ensure_all_started(owls),
	erltipbase_sup:start_link().

stop(_State) ->
	ok.
