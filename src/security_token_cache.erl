%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2023, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 02. Feb 2023 10:49 p.m.
%%%-------------------------------------------------------------------
-module(security_token_cache).
-author("stephb").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-export([creation_info/0,get_token/1,add_token/3,delete_token/1]).

-define(SERVER, ?MODULE).

-record(cache_entry,{
	email = [] ::string(),
	userinfo,
	ts = os:system_time(second)
}).
-record(security_token_cache_state, { cache = #{} }).
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

get_token(Token) ->
	gen_server:call(?MODULE, { get , Token }).

add_token(Token,EMail,UserInfo) ->
	gen_server:call(?MODULE, { add , Token, EMail, UserInfo}).

delete_token(Token) ->
	gen_server:call(?MODULE, { delete , Token}).


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
	{ok, State :: #security_token_cache_state{}} | {ok, State :: #security_token_cache_state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([]) ->
	{ok, #security_token_cache_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #security_token_cache_state{}) ->
	                 {reply, Reply :: term(), NewState :: #security_token_cache_state{}} |
	                 {reply, Reply :: term(), NewState :: #security_token_cache_state{}, timeout() | hibernate} |
	                 {noreply, NewState :: #security_token_cache_state{}} |
	                 {noreply, NewState :: #security_token_cache_state{}, timeout() | hibernate} |
	                 {stop, Reason :: term(), Reply :: term(), NewState :: #security_token_cache_state{}} |
	                 {stop, Reason :: term(), NewState :: #security_token_cache_state{}}).
handle_call({get,Token}, _From, State) ->
	case maps:get(Token,State#security_token_cache_state.cache,undefined) of
		undefined ->
			{ reply, undefined , State };
		Entry ->
			{ reply, {Entry#cache_entry.email, Entry#cache_entry.userinfo}, State}
	end;
handle_call({delete,Token}, _From, State) ->
	{reply, ok, State#security_token_cache_state{ cache = maps:remove(Token,State#security_token_cache_state.cache)}};
handle_call({add,Token, ENail, UserInfo}, _From, State) ->
	{reply, ok, State#security_token_cache_state{
		cache = maps:put(Token,#cache_entry{ email = ENail, userinfo = UserInfo},State#security_token_cache_state.cache)}};
handle_call(_Request, _From, State = #security_token_cache_state{}) ->
	{reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #security_token_cache_state{}) ->
	{noreply, NewState :: #security_token_cache_state{}} |
	{noreply, NewState :: #security_token_cache_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #security_token_cache_state{}}).
handle_cast(_Request, State = #security_token_cache_state{}) ->
	{noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #security_token_cache_state{}) ->
	{noreply, NewState :: #security_token_cache_state{}} |
	{noreply, NewState :: #security_token_cache_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #security_token_cache_state{}}).
handle_info(_Info, State = #security_token_cache_state{}) ->
	{noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #security_token_cache_state{}) -> term()).
terminate(_Reason, _State = #security_token_cache_state{}) ->
	ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #security_token_cache_state{},
                  Extra :: term()) ->
	                 {ok, NewState :: #security_token_cache_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #security_token_cache_state{}, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
