-module(erltipbase_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Processes =
		registry:creation_info() ++
		microservice_kafka_handler:creation_info() ++
		microservice:creation_info() ++
		mgr_external_webserver:creation_info() ++
		mgr_internal_webserver:creation_info() ++
		rest_server_process:creation_info() ++
		security_sdk:creation_info(),
	{ok, {{one_for_one, 1, 5}, Processes}}.
