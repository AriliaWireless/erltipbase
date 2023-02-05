%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2023, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 03. Feb 2023 10:48 p.m.
%%%-------------------------------------------------------------------
-module(ws_user_registry).
-author("stephb").

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([creation_info/0, add_user/2, delete_pid/1, delete_user/1, send_frame/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(ws_user_registry_state, {
	pid_to_user = #{},
	user_to_pid =#{}
}).

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

-spec add_user( EMail::binary(), WSPid::pid()) -> { ok }.
add_user(EMail, Pid) ->
	gen_server:call(?MODULE, { add , EMail, Pid }).

-spec delete_user( EMail::binary() ) -> {ok}.
delete_user(EMail) ->
	gen_server:call(?MODULE, { delete_user , EMail}).

-spec delete_pid(WSPid::pid()) -> {ok}.
delete_pid(Pid) ->
	gen_server:call(?MODULE, { delete_pid , Pid}).

-spec send_frame(EMail::binary(), Frame::{ FrameType :: binary | text , FrameContent::binary()}) -> { ok, NumberOfMessagesSent::integer() }._
send_frame(Email,Frame) ->
	gen_server:call(?MODULE, { send_frame , Email, Frame}).

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
	{ok, State :: #ws_user_registry_state{}} | {ok, State :: #ws_user_registry_state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([]) ->
	{ok, #ws_user_registry_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #ws_user_registry_state{}) ->
	                 {reply, Reply :: term(), NewState :: #ws_user_registry_state{}} |
	                 {reply, Reply :: term(), NewState :: #ws_user_registry_state{}, timeout() | hibernate} |
	                 {noreply, NewState :: #ws_user_registry_state{}} |
	                 {noreply, NewState :: #ws_user_registry_state{}, timeout() | hibernate} |
	                 {stop, Reason :: term(), Reply :: term(), NewState :: #ws_user_registry_state{}} |
	                 {stop, Reason :: term(), NewState :: #ws_user_registry_state{}}).
handle_call({ add , EMail, Pid }, _From, State) ->
	%% add to user
	NewUsers = case maps:get(EMail,State#ws_user_registry_state.user_to_pid,undefined) of
		undefined ->
			maps:put(EMail,[Pid],State#ws_user_registry_state.user_to_pid);
		PidList ->
			maps:put(EMail,PidList ++ Pid,State#ws_user_registry_state.user_to_pid)
	end,
	{reply, ok, State#ws_user_registry_state{
		user_to_pid = NewUsers,
		pid_to_user = maps:put(Pid,EMail,State#ws_user_registry_state.pid_to_user)}};
handle_call({ delete_user , EMail}, _From, State = #ws_user_registry_state{}) ->
	NewUsers = maps:remove(EMail,State#ws_user_registry_state.user_to_pid),
	Filter = fun(_Key, Value) -> Value =/= EMail end,
	NewPids = maps:filter(Filter, State#ws_user_registry_state.pid_to_user),
	{reply, ok, State#ws_user_registry_state{ user_to_pid = NewUsers, pid_to_user = NewPids}};
handle_call({ delete_pid , Pid}, _From, State = #ws_user_registry_state{}) ->
	NewUsers = case maps:get(Pid,State#ws_user_registry_state.pid_to_user,undefined) of
		undefined ->
			Fun = fun(K,V,Acc) ->
					maps:put(K,lists:delete(Pid,V),Acc)
				end,
			maps:fold(Fun,#{},State#ws_user_registry_state.user_to_pid);
		EMail ->
			case maps:get(EMail,State#ws_user_registry_state.user_to_pid,undefined) of
				undefined ->
					State#ws_user_registry_state.user_to_pid;
				Pids ->
					maps:put(EMail, lists:delete(Pid,Pids), State#ws_user_registry_state.user_to_pid)
			end
	end,
	NewPids = maps:remove(Pid,State#ws_user_registry_state.pid_to_user),
	{reply, ok, State#ws_user_registry_state{ user_to_pid = NewUsers, pid_to_user = NewPids}};
handle_call( {send_frame, EMail, Frame}, _From, State = #ws_user_registry_state{}) ->
	case maps:get(EMail,State#ws_user_registry_state.user_to_pid,undefined) of
		undefined ->
			{reply,{ok,0},State};
		PidList ->
			Fun = fun(Pid) -> Pid ! Frame end,
			lists:foreach(Fun,PidList),
			{reply, {ok,length(PidList)}, State}
	end;
handle_call(_Request, _From, State = #ws_user_registry_state{}) ->
	{reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #ws_user_registry_state{}) ->
	{noreply, NewState :: #ws_user_registry_state{}} |
	{noreply, NewState :: #ws_user_registry_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #ws_user_registry_state{}}).
handle_cast(_Request, State = #ws_user_registry_state{}) ->
	{noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #ws_user_registry_state{}) ->
	{noreply, NewState :: #ws_user_registry_state{}} |
	{noreply, NewState :: #ws_user_registry_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #ws_user_registry_state{}}).
handle_info(_Info, State = #ws_user_registry_state{}) ->
	{noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #ws_user_registry_state{}) -> term()).
terminate(_Reason, _State = #ws_user_registry_state{}) ->
	ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #ws_user_registry_state{},
                  Extra :: term()) ->
	                 {ok, NewState :: #ws_user_registry_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #ws_user_registry_state{}, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
