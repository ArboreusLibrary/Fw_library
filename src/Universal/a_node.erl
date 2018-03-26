%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Erlang nodes handler extension
%%%
%%% @end
%%% Created : 07. Март 2018 18:48
%%%-------------------------------------------------------------------
-module(a_node).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% API
-export([
	test/0,
	change_name/2
]).



%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Change node name
-spec change_name(New_name,Net_mode) -> {error,_Reason} | {ok,_Node_name}
	when
		New_name :: atom() | unicode:chardata(),
		Net_mode :: shortnames | longnames.

change_name(New_name,Net_mode) when is_atom(New_name) ->
	change_name(atom_to_list(New_name),Net_mode);
change_name(New_name,Net_mode) when is_list(New_name) ->
	case node() of
		'nonode@nohost' -> {error,no_net_kernel};
		_Node_name ->
			case Net_mode of
				shortnames ->
					ok = net_kernel:stop(),
					{ok,_Net_kernel_pid} = net_kernel:start([
						list_to_atom(New_name),Net_mode
					]),
					{ok,node()};
				_ ->
					ok = net_kernel:stop(),
					{ok,_Net_kernel_pid} = net_kernel:start([
						list_to_atom(lists:concat([New_name,"@",net_adm:localhost()])),
						Net_mode
					]),
					{ok,node()}
			end
	end.
