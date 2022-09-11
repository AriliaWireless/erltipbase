-module(utils).
-export([to_hex/1]).

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




