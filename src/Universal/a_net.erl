%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2016, http://arboreus.system
%%% @doc Network functionality handler
%%%
%%% @end
%%% Created : 19. Янв. 2016 17:47
%%%-------------------------------------------------------------------
-module(a_net).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").
-vsn("388").

%% API
-export([
	ipv4_to_integer/1,
	ipv6_to_integer/1,
	integer_to_ipv4/2
]).

%% Module Include Start
-include("../Handler/a.hrl").
-include("../Handler/types_network.hrl").
%% Module Include End


%%-----------------------------------
%% @doc Return integer from IPv4
-spec ipv4_to_integer(Ip) -> ipv4_integer() | {error,_Reason}
	when
		Ip :: ipv4_tuple() | ipv4_list() | ipv4_string().

ipv4_to_integer({A,B,C,D}) -> ipv4_to_integer([A,B,C,D]);
ipv4_to_integer([A,B,C,D])
	when
		is_integer(A),is_integer(B),
		is_integer(C),is_integer(D),
		A >= 0, A =< 255, B >= 0, B =< 255,
		C >= 0, C =< 255, D >= 0, D =< 255 ->
	(A*16777216)+(B*65536)+(C*256)+(D);
ipv4_to_integer(Ip_string) when is_list(Ip_string) ->
	case io_lib:char_list(Ip_string) of
		true ->
			try
				{ok,Ip_tuple} = inet:parse_ipv4_address(Ip_string),
				ipv4_to_integer(Ip_tuple)
			catch _:_ -> a:error(?NAME_FUNCTION(),a018) end;
		_ -> a:error(?NAME_FUNCTION(),a003)
	end;
ipv4_to_integer(_) -> a:error(?NAME_FUNCTION(),a003).


%%-----------------------------------
%% @doc Return integer from IPv6
-spec ipv6_to_integer(Ip) -> ipv4_integer() | {error,_Reason}
	when
		Ip :: ipv6_string().

ipv6_to_integer(Ip_string) ->
	case inet:parse_ipv6_address(Ip_string) of
		{ok,_} -> list_to_integer(re:replace(Ip_string,":","",[global,{return,list}]),16);
		_ -> a:error(?NAME_FUNCTION(),a018)
	end.


%%-----------------------------------
%% @doc Return formatted Ip address from integer
-spec integer_to_ipv4(Integer,Output_type) ->
	ipv4_tuple() | ipv4_list() | ipv4_binary() | ipv4_string() | {error,_Reason}
	when
		Integer :: ipv4_integer(),
		Output_type :: tuple | list | binary | string.

integer_to_ipv4(Integer,Output_type) when is_integer(Integer), Integer >= 0 ->
	A = Integer div 16777216, After_A = Integer-A*16777216,
	B = After_A div 65536, After_B = After_A-B*65536,
	C = After_B div 256,
	D = After_B-C*256,
	if
		A > 255 -> a:error(?NAME_FUNCTION(),a009);
		B > 255 -> a:error(?NAME_FUNCTION(),a009);
		C > 255 -> a:error(?NAME_FUNCTION(),a009);
		D > 255 -> a:error(?NAME_FUNCTION(),a009);
		true ->
			case Output_type of
				tuple -> {A,B,C,D};
				list -> [A,B,C,D];
				binary ->
					<<(integer_to_binary(A))/binary,"."/utf8,
						(integer_to_binary(B))/binary,"."/utf8,
						(integer_to_binary(C))/binary,"."/utf8,
						(integer_to_binary(D))/binary>>;
				_ ->
					lists:concat([
						integer_to_list(A),".",
						integer_to_list(B),".",
						integer_to_list(C),".",
						integer_to_list(D)
					])
			end
	end.