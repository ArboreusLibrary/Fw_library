%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2016, http://arboreus.system
%%% @doc
%%%
%%% @end
%%% Created : 19. Янв. 2016 17:47
%%%-------------------------------------------------------------------
-module(a_net).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").
-vsn("0.0.3.194").

%% API
-export([
	ipv4_to_integer/1,
	ipv6_to_integer/1
]).

%% Module Include Start
-include("../Handler/a.hrl").
%% Module Include End


%%-----------------------------------
%% @doc Return integer from IPv4
-spec ipv4_to_integer(Ip) -> integer() | {error,_Reason}
	when
		Ip :: tuple() | list() | string().

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
			catch _:_ -> a:error(?FUNCTION_NAME(),a018) end;
		_ -> a:error(?FUNCTION_NAME(),a003)
	end;
ipv4_to_integer(_) -> a:error(?FUNCTION_NAME(),a003).


%%-----------------------------------
%% @doc Return integer from IPv6
-spec ipv6_to_integer(Ip::string()) -> integer() | {error,_Reason}.

ipv6_to_integer(Ip_string) ->
	case inet:parse_ipv6_address(Ip_string) of
		{ok,_} -> list_to_integer(re:replace(Ip_string,":","",[global,{return,list}]),16);
		_ -> a:error(?FUNCTION_NAME(),a018)
	end.
