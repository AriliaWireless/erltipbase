-module(security_sdk).
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

-export([creation_info/0,validate_token/1]).

-record(state, {
}).

%% API.
creation_info() ->
	[	#{	id => ?MODULE ,
	       start => { ?MODULE , start_link, [] },
	       restart => permanent,
	       shutdown => 100,
	       type => worker,
	       modules => [?MODULE]} ].

validate_token(T) when is_binary(T) ->
	gen_server:call(?MODULE, { validate_token , T }).

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

%% gen_server.

init([]) ->
	{ok, #state{}}.

handle_call({validate_token,Token}, _From, State) ->
	case security_token_cache:get_token(Token) of
		undefined ->
			{ok,SecurityServices} = microservice:get_service_info(<<"owsec">>),
			SecurityService = utils:get_first(SecurityServices),
			#{ <<"privateEndPoint">> := SecurityServiceEndPoint } = SecurityService,
			#{ <<"key">> := SecurityKey } = SecurityService,
			{ok,MyServiceInfo} = microservice:get_my_service_info(),
			#{ <<"publicEndPoint">> := MyPublicName} = MyServiceInfo,
			Request = { binary_to_list(SecurityServiceEndPoint) ++ "/api/v1/validateToken?token=" ++ binary_to_list(Token),
				[ {"X-API-KEY" , binary_to_list(SecurityKey) }, {"X-INTERNAL-NAME", binary_to_list(MyPublicName)}]},
			case httpc:request(get,Request,[],[]) of
				{ok, {{ _, ReturnCode, _}, _, Body }} ->
					Answer = jsone:decode(list_to_binary(Body)),
					case ReturnCode of
						200 ->
							#{ <<"userInfo">> := UserInfo } = Answer,
							#{ <<"email">> := EMail } = UserInfo,
							security_token_cache:add_token(Token,EMail,UserInfo),
							{reply, {ok, EMail, UserInfo } , State};
						_ ->
							{ reply, {error, ReturnCode}, State}
					end;
				_ ->
					{ reply, {error, 500}, State}
			end;
		{EMail,Userinfo} ->
			{reply, {ok, EMail, Userinfo } , State}
	end;

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
