-module(registry).
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
-export([creation_info/0, get/1, put/2]).

-record(state, {
	file = "" ::string(),
	registry = dict:new()
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
	gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

%% gen_server.
save_registry( State ) ->
	Format = fun(Term) -> io_lib:format("~tp.~n", [Term]) end,
	Text = unicode:characters_to_binary(lists:map(Format, dict:to_list(State#state.registry))),
	file:write_file(State#state.file, Text).

get( Key ) ->
	gen_server:call(?MODULE, {get, Key}).

put( Key, Value ) ->
	gen_server:call(?MODULE, {put, Key, Value}).

init([]) ->
	process_flag(trap_exit, true),
	{ok, DataDir } = application:get_env(data_dir),
	FileName =binary:bin_to_list(DataDir) ++ "/registry",
	case filelib:is_file(FileName) of
		true ->
			{ok, Content} = file:consult(FileName),
			NewDict = dict:from_list(Content),
      {ok, #state{ file = FileName , registry = NewDict} };
		false ->
			Content = [ {system_id, rand:uniform(576460752303423481)}],
			NewDict = dict:from_list(Content),
			NewState = #state{ file = FileName , registry = NewDict},
			save_registry(NewState),
			{ok, NewState}
	end.

handle_call({get, Key}, _From, State) ->
	case dict:find(Key, State#state.registry) of
		error ->
			{ reply, undefined , State };
		{ok, Value} ->
			{ reply, Value, State}
	end;

handle_call({put, Key, Value}, _From, State) ->
	NewState = State#state{ registry = dict:store(Key,Value,State#state.registry)},
	save_registry(NewState),
	{ reply, ok , NewState};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
