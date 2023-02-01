-module(utils).

-export([to_hex/1,get_first/1, set_app_name/1, get_app_name/0, add_cors/2]).
-export([number_of_cpus/0]).

to_hex_digit(D) ->
	case D>9 of
		true ->
			D - 10 + $a;
		false ->
			D + $0
	end.

to_hex(D) when is_binary(D) ->
	to_hex(D,<<>>).

to_hex(<<>>,Acc) ->
	Acc;
to_hex(<<N1:4,N2:4,Rest/binary>>,Acc) ->
	D1 = to_hex_digit(N1),
	D2 = to_hex_digit(N2),
	to_hex(Rest,<<Acc/binary,D1:8,D2:8>>).

get_first(M) when is_map(M) ->
	I = maps:iterator(M),
	{ _, V, _} = maps:next(I),
	V.

-spec set_app_name( Name::atom() ) -> ok.
set_app_name(AppName) ->
	persistent_term:put(running_app,AppName).

-spec get_app_name() -> Name::atom().
get_app_name() ->
	persistent_term:get(running_app).

-spec number_of_cpus() -> integer().
number_of_cpus() ->
	length(cpu_sup:util([detailed,per_cpu])).

add_cors(Req0, Methods) ->
	Req1 = cowboy_req:set_resp_header(<<"access-control-allow-credentials">>, <<"true">>, Req0),
	Req2 = cowboy_req:set_resp_header(<<"access-control-request-headers">>, <<"*">>, Req1),
	Req3 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req2),
	Req4 = cowboy_req:set_resp_header(<<"vary">>, <<"origin, accept-encoding">>, Req3),
	Req5 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, Methods, Req4),
	cowboy_req:set_resp_header(<<"access-control-max-age">>, <<"20">>, Req5).
