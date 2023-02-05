%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2022, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 22. Dec 2022 10:00 a.m.
%%%-------------------------------------------------------------------
-module(rest_server_process).
-author("stephb").

-behaviour(gen_server).

-define(RESTAPI_SERVER_NAME,restapi_server).
%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3, creation_info/0]).

-define(SERVER, ?MODULE).

-record(rest_server_process_state, { listener_pid }).

%%%===================================================================
%%% API
%%%===================================================================
creation_info() ->
    [	#{	id => ?MODULE ,
           start => { ?MODULE , start_link, [] },
           restart => permanent,
           shutdown => 100,
           type => worker,
           modules => [?MODULE]} ].

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
	{ok, State :: #rest_server_process_state{}} | {ok, State :: #rest_server_process_state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([]) ->
	{ok,U1} = application:get_env(utils:get_app_name(),restapi_external_uri),
	URI = uri_string:parse(U1),
	Port = maps:get(port,URI),
	Dispatch = cowboy_router:compile([
				                                 {'_', [
					                                  {"/api/v1/system", restapi_system_command, [ test ]},
				                                    {"/api/v1/ws", restapi_ws_handler, [ ws_test ]}
				                                 ]}
			                                 ]),
	{ok, Pid} = case application:get_env(utils:get_app_name(),restapi_external_secure,true) of
							true ->
								{ok, CertFile} = application:get_env(utils:get_app_name(),restapi_external_cert),
								{ok, KeyFile} = application:get_env(utils:get_app_name(),restapi_external_key),
								Password = application:get_env(utils:get_app_name(),restapi_external_key_password,""),
								cowboy:start_tls(?RESTAPI_SERVER_NAME, [
									{port, Port},
									{certfile, code:priv_dir(utils:get_app_name()) ++ "/" ++ CertFile},
									{keyfile, code:priv_dir(utils:get_app_name()) ++ "/" ++ KeyFile},
									{password,Password}
								], #{
									                 env => #{dispatch => Dispatch}
								                 });
							false ->
								cowboy:start_clear(?RESTAPI_SERVER_NAME,
								                   [{port, Port}, inet, inet6],
								                   #{env => #{dispatch => Dispatch}})
	end,
	process_flag(trap_exit, true),
	%% io:format("WebServer: ~p~n",[Pid]),
	{ok, #rest_server_process_state{ listener_pid = Pid}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #rest_server_process_state{}) ->
	                 {reply, Reply :: term(), NewState :: #rest_server_process_state{}} |
	                 {reply, Reply :: term(), NewState :: #rest_server_process_state{}, timeout() | hibernate} |
	                 {noreply, NewState :: #rest_server_process_state{}} |
	                 {noreply, NewState :: #rest_server_process_state{}, timeout() | hibernate} |
	                 {stop, Reason :: term(), Reply :: term(), NewState :: #rest_server_process_state{}} |
	                 {stop, Reason :: term(), NewState :: #rest_server_process_state{}}).
handle_call(_Request, _From, State = #rest_server_process_state{}) ->
	{reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #rest_server_process_state{}) ->
	{noreply, NewState :: #rest_server_process_state{}} |
	{noreply, NewState :: #rest_server_process_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #rest_server_process_state{}}).
handle_cast(_Request, State = #rest_server_process_state{}) ->
	{noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #rest_server_process_state{}) ->
	{noreply, NewState :: #rest_server_process_state{}} |
	{noreply, NewState :: #rest_server_process_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #rest_server_process_state{}}).
handle_info(_Info, State = #rest_server_process_state{}) ->
	{noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #rest_server_process_state{}) -> term()).
terminate(_Reason, _State = #rest_server_process_state{}) ->
	cowboy:stop_listener(?RESTAPI_SERVER_NAME),
	ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #rest_server_process_state{},
                  Extra :: term()) ->
	                 {ok, NewState :: #rest_server_process_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #rest_server_process_state{}, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
