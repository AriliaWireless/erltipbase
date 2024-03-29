-module(utils).
-include_lib("public_key/include/public_key.hrl").
-export([to_hex/1,get_first/1, set_app_name/1, get_app_name/0]).
-export([number_of_cpus/0]).
-export([certificate_expiry/1]).

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

-spec certificate_expiry(Filename::binary()) -> integer().
certificate_expiry(FileName) ->
	{ok,PemBin} = file:read_file(FileName),
	[RawCert] = public_key:pem_decode(PemBin),
	Cert = public_key:pem_entry_decode(RawCert),
	{utcTime, T} = Cert#'Certificate'.tbsCertificate#'TBSCertificate'.validity#'Validity'.notAfter,
	UTC = {{2000+list_to_integer(string:sub_string(T,1,2)),
	        list_to_integer(string:sub_string(T,3,4)),
	        list_to_integer(string:sub_string(T,5,6))},
	       {list_to_integer(string:sub_string(T,7,8)),
	        list_to_integer(string:sub_string(T,9,10)),
	        list_to_integer(string:sub_string(T,11,12))}},
	date_util:datetime_to_epoch(UTC).

