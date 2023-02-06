%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2023, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 05. Feb 2023 3:05 p.m.
%%%-------------------------------------------------------------------
-module(main).
-author("stephb").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
-export([creation_info/0]).
-define(SERVER, ?MODULE).

-record(main_state, {}).

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
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
	{ok, State :: #main_state{}} | {ok, State :: #main_state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([]) ->
	ws_user_registry:add_callback(ws_process,process),
	_E = logger:add_handler(ws_logging,ws_logger,#{}),
	logger:set_application_level(utils:get_app_name(),all),
	{ok, #main_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #main_state{}) ->
	                 {reply, Reply :: term(), NewState :: #main_state{}} |
	                 {reply, Reply :: term(), NewState :: #main_state{}, timeout() | hibernate} |
	                 {noreply, NewState :: #main_state{}} |
	                 {noreply, NewState :: #main_state{}, timeout() | hibernate} |
	                 {stop, Reason :: term(), Reply :: term(), NewState :: #main_state{}} |
	                 {stop, Reason :: term(), NewState :: #main_state{}}).
handle_call(_Request, _From, State = #main_state{}) ->
	{reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #main_state{}) ->
	{noreply, NewState :: #main_state{}} |
	{noreply, NewState :: #main_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #main_state{}}).
handle_cast(_Request, State = #main_state{}) ->
	{noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #main_state{}) ->
	{noreply, NewState :: #main_state{}} |
	{noreply, NewState :: #main_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #main_state{}}).
handle_info(_Info, State = #main_state{}) ->
	{noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #main_state{}) -> term()).
terminate(_Reason, _State = #main_state{}) ->
	ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #main_state{},
                  Extra :: term()) ->
	                 {ok, NewState :: #main_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #main_state{}, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
