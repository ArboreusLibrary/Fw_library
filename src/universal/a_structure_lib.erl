%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Arboreus structures library
%%%
%%% @end
%%% Created : 07/02/2018 at 14:47
%%%-------------------------------------------------------------------
-module(a_structure_lib).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Constants

%% Data types
-include("../data_models/types/types_general.hrl").

%% Data models

%% API
-export([
	test/0,
	reference/2,reference/3,reference/4,
	sort_handler/6
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() ->
	Time_start = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_structure_lib) testing started at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_start), Time_start]
	),
	Time_stop = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_structure_lib) testing finished at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_stop), Time_stop]
	),
	io:format("Test time is: ~p~n", [Time_stop - Time_start]),
	ok.


%% ----------------------------
%% @doc Sorting structures procedure handler
-spec sort_handler(Module,Positions,Check,Structures,Smaller,Larger) -> list()
	when
	Module :: module(),
	Positions :: list_of_integers(),
	Check :: list_of_values(),
	Structures :: list() | proplists:proplist() | record() | tuple() | map() | gb_trees:tree(),
	Smaller :: list(),
	Larger :: list().

sort_handler(Module,Positions,Check,[Structure|Structures],Smaller,Larger) ->
	Structure_check = Module:sorting_elements_handler(Positions,Structure,[]),
	case Structure_check =< Check of
		true ->
			sort_handler(
				Module,Positions,Check,Structures,
				[Structure|Smaller],Larger
			);
		false ->
			sort_handler(
				Module,Positions,Check,Structures,
				Smaller,[Structure|Larger]
			)
	end;
sort_handler(_,_,_,[],Smaller,Larger) -> {Smaller,Larger}.


%% ----------------------------
%% @doc Wrapper for reference/2
-spec reference(Module,Structures) -> false | {true,Reference}
	when
	Module :: a_structure_r | a_structure_l | a_structure_t | a_structure_m |
	a_structure_gb | a_structure_pl,
	Structures :: list(),
	Reference :: proplists:proplist().

reference(Module,Structures) -> reference(Module,Structures,all).


%% ----------------------------
%% @doc Wrapper for reference/3
-spec reference(Module,Structures,Positions) -> false | {true,Reference}
	when
	Module :: a_structure_r | a_structure_l | a_structure_t | a_structure_m |
	a_structure_gb | a_structure_pl,
	Structures :: list(),
	Positions :: list() | all,
	Reference :: proplists:proplist().

reference(Module,Structures,Positions) ->
	reference(Module,Structures,Positions,[]).


%% ----------------------------
%% @doc Generate reference
-spec reference(Module,Structures,Positions,Reference) ->
	false | {true,Reference}
	when
	Module :: a_structure_r | a_structure_l | a_structure_t | a_structure_m |
	a_structure_gb | a_structure_pl,
	Structures :: list(),
	Positions :: list() | {all,Length},
	Length :: pos_integer(),
	Reference :: proplists:proplist().

reference(Module,Structures,{all,Length},Reference) ->
	[Etalon|_] = Structures,
	reference_handler(
		Module,Structures,Module:model(verificator,Etalon),
		lists:seq(1,Length),Reference
	);
reference(Module,Structures,Positions,Reference) ->
	[Etalon|_] = Structures,
	reference_handler(
		Module,Structures,Module:model(verificator,Etalon),
		Positions,Reference
	).


%% ----------------------------
%% @doc Generating reference procedure handler
-spec reference_handler(Module,Structures,Model,Positions,Reference) ->
	false | {true,Reference}
	when
	Module :: a_structure_r | a_structure_l | a_structure_t | a_structure_m |
		a_structure_gb | a_structure_pl,
	Structures :: list(),
	Model :: list(),
	Positions :: list_of_integers(),
	Reference :: proplists:proplist().

reference_handler(_,[],_,_,Reference) -> {true,Reference};
reference_handler(Module,[Structure|Structures],Model,Positions,Reference) ->
	case Module:verify(return_boolean,Model,Structure) of
		true ->
			Compose_reference = fun
				Function([],Reference_income) -> Reference_income;
				Function([{Key,Value}|List],Reference_income) ->
					case proplists:get_value(Key,Reference_income) of
						undefined ->
							Function(List,lists:append(Reference_income,[{Key,[Value]}]));
						Reference_value ->
							Function(List,lists:keyreplace(
								Key,1,Reference_income,{Key,begin
									case lists:member(Value,Reference_value) of
										false -> lists:append(Reference_value,[Value]);
										_ -> Reference_value
									end
								end
								}))
					end
			end,
			reference_handler(
				Module,Structures,Model,Positions,
				Compose_reference(Module:elements(Positions,Structure),Reference)
			);
		Verification_result -> Verification_result
	end.