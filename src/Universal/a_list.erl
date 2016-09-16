%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV (http://alexandr.kirilov.me)
%%% @copyright (C) 2015, Arboreus, (http://arboreus.systems)
%%% @doc
%%%
%%% @end
%%% Created : 21. Jul 2015 21:55
%%%-------------------------------------------------------------------
-module(a_list).
-author("Alexandr KIRILOV (http://alexandr.kirilov.me)").

%% API
-export([
	test/0,
	get_out/3,
	check/2,check/3,
	clear_duplicates/1,
	find_members/2
]).

%% Module Include Start
-include("../Handler/a.hrl").
%% Module Include End


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Find members of list from another list
-spec find_members(Members,List) -> list()
	when
		Members :: list(),
		List :: list().

find_members(Members_list,List) ->
	find_members(Members_list,List,[]).


%% ----------------------------
%% @doc Find members of list from another list
-spec find_members(Members,List,Output) -> list()
	when
		Members :: list(),
		List :: list(),
		Output :: list().

find_members([],_,Output) -> Output;
find_members([Member|Members],List,Output) ->
	find_members(Members,List,
		case lists:member(Member,List) of
			true -> lists:append(Output,[Member]);
			false -> Output
		end
	).


%% ----------------------------
%% @doc Clear duplicates from defined list
-spec clear_duplicates(List::list()) -> list().

clear_duplicates(List) -> clear_duplicates(List,[]).


%% ----------------------------
%% @doc Clear duplicates from defined list
-spec clear_duplicates(List,Output) -> list()
	when
		List :: list(),
		Output :: list().

clear_duplicates([],Output) -> Output;
clear_duplicates([Element|List],Output) ->
	clear_duplicates(
		List,
		case lists:member(Element,Output) of
			true -> Output;
			false -> lists:append(Output,[Element])
		end
	).

%% ----------------------------
%% @doc Wrapper function for check/3, checking list of typed elements
-spec check(List,Type_properties) -> list() | nomatch
	when
		List :: list(),
		Type_properties :: {Type,Type_parameters},
		Type :: atom(),
		Type_parameters :: list().

check(List,Type_properties) -> check(List,Type_properties,[]).


%% ----------------------------
%% @doc Checking list of typed elements
-spec check(List,Type_properties,Output) -> list() | nomatch
	when
		List :: list(),
		Type_properties :: {Type,Type_parameters},
		Type :: atom(),
		Type_parameters :: list(),
		Output :: list().

check([],_,Output) -> Output;
check([Element|List],{Type,Type_parameters},Output) ->
	case a_params:check(Type,Element,Type_parameters) of
		nomatch -> nomatch;
		Checked_element ->
			check(
				List,{Type,Type_parameters},
				lists:append(Output,[Checked_element])
			)
	end.


%% ----------------------------
%% @doc Get out key-value pair and return cleared List and value
-spec get_out(Type,Key,List) -> Result | {error,_Reason}
	when
		Type :: value | pair,
		Key :: atom(),
		List :: list(),
		Result :: list().

get_out(value,Key,List) ->
	Value = proplists:get_value(Key,List),
	case Value of
		undefined -> a:error(?FUNCTION_NAME(),m004_001);
		_ ->
			List_out = proplists:delete(Key,List),
			[Value,List_out]
	end;
get_out(pair,Key,List) ->
	Value = proplists:get_value(Key,List),
	case Value of
		undefined -> a:error(?FUNCTION_NAME(),m004_001);
		_ ->
			List_out = proplists:delete(Key,List),
			[{Key,Value},List_out]
	end;
get_out(_,_,_) -> a:error(?FUNCTION_NAME(),a000).