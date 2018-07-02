%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Arboreus list based data structures handler
%%%
%%% @end
%%% Created : 06/21/2018 at 10:27
%%%-------------------------------------------------------------------
-module(a_structure_l).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Constants

%% Data types
-include("../data_models/types/types_general.hrl").

%% Data models

%% API
-export([
	test/0,
	verify/3,
	mass_verify/2,mass_verify/3,
	model/2,
	reference/1,reference/2,reference/3,
	elements/2
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() ->
	Time_start = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_structure_l) testing started at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_start), Time_start]
	),
	List1 = [1,atom,0.1,"11"],
	List2 = [2,second_atom,0.1,"11"],
	List3 = [3,third_atom,0.1,"22"],
	List_wrong = [one,atom,0.1,"123"],
	Model1 = [(fun is_integer/1),(fun is_atom/1),(fun is_float/1),(fun is_list/1)],
	true = verify(return_boolean,Model1,List1),
	true = verify(return_boolean,Model1,List2),
	{true,List1} = verify(return_structure,Model1,List1),
	false = verify(return_boolean,Model1,List_wrong),
	false = verify(return_boolean,[],List1),
	io:format("DONE! Fun verify/3 test passed~n"),
	Model2 = model(verificator,List1),
	false = verify(return_boolean,Model2,List_wrong),
	true = verify(return_boolean,Model2,List2),
	io:format("DONE! Fun model/2 test passed~n"),
	Structures = [List1,List2,List3],
	Structures_wrong = [List1,List2,List_wrong],
	true = mass_verify(Model1,Structures),
	true = mass_verify(Model2,Structures),
	false = mass_verify(Model1,Structures_wrong),
	false = mass_verify(Model2,Structures_wrong),
	false = mass_verify(Model1,[]),
	true = mass_verify([],[]),
	false = mass_verify([],Structures),
	io:format("DONE! Fun mass_verify/2 test passed~n"),
	{true,Structures} = mass_verify(return_list,Model1,Structures),
	{true,Structures} = mass_verify(return_list,Model2,Structures),
	io:format("DONE! Fun mass_verify/3 test passed~n"),
	{true,Reference1} = reference(Structures),
	{true,Reference1} = reference(Structures,all),
	{true,Reference1} = reference(Structures,all,[]),
	{true,Reference1} = reference(Structures,[1,2,3,4],[]),
	io:format("DONE! Fun reference/3 test passed: ~p~n",[Reference1]),
	Time_stop = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_structure_l) testing finished at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_stop), Time_stop]
	),
	io:format("Test time is: ~p~n", [Time_stop - Time_start]),
	ok.


%% ----------------------------
%% @doc Wrapper for reference/2
-spec reference(Structures) -> false | {true,Reference}
	when
	Structures :: list(),
	Reference :: proplists:proplist().

reference(Structures) -> reference(Structures,all).


%% ----------------------------
%% @doc Wrapper for reference/3
-spec reference(Structures,Positions) -> false | {true,Reference}
	when
	Structures :: list(),
	Positions :: list() | all,
	Reference :: proplists:proplist().
	
reference(Structures,Positions) ->
	reference(Structures,Positions,[]).

%% ----------------------------
%% @doc Generate reference
-spec reference(Structures,Positions,Reference) ->
	false | {true,Reference}
	when
	Structures :: list(),
	Positions :: list() | all,
	Reference :: proplists:proplist().

reference(Structures,all,Reference) ->
	[Etalon|_] = Structures,
	reference_handler(
		Structures,model(verificator,Etalon),
		lists:seq(1,length(Etalon)),Reference
	);
reference(Structures,Positions,Reference) ->
	[Etalon|_] = Structures,
	reference_handler(
		Structures,model(verificator,Etalon),
		Positions,Reference
	).


%% ----------------------------
%% @doc Generating reference procedure handler
-spec reference_handler(Structures,Model,Positions,Reference) ->
	false | {true,Reference}
	when
	Structures :: list(),
	Model :: list(),
	Positions :: list_of_integers(),
	Reference :: proplists:proplist().

reference_handler([],_,_,Reference) -> {true,Reference};
reference_handler([Structure|Structures],Model,Positions,Reference) ->
	case verify(return_boolean,Model,Structure) of
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
				Structures,Model,Positions,
				Compose_reference(elements(Positions,Structure),Reference)
			);
		Verification_result -> Verification_result
	end.


%% ----------------------------
%% @doc Wrapper for elements/3
-spec elements(Positions,Structure) -> proplists:proplist()
	when
	Positions :: list_of_integers(),
	Structure :: list().

elements(Positions,Structure) ->
	elements(Positions,Structure,[]).


%% ----------------------------
%% @doc Return proplist within position-value pair of the structure
-spec elements(Positions,Structure,Elements) -> proplists:proplist()
	when
	Positions :: list_of_integers(),
	Structure :: list(),
	Elements :: proplists:proplist().

elements([],_,Elements) -> Elements;
elements([Position|Positions],Structure,Elements) ->
	elements(
		Positions,Structure,
		lists:append(Elements,[
			{Position,lists:nth(Position,Structure)}
		])
	).


%% ----------------------------
%% @doc Return data model of the structure
-spec model(Kind,Structure) -> list_of_functions() | list_of_atoms()
	when
	Kind :: verificator | description,
	Structure :: list_of_values().

model(Kind,Structure) ->
	[a_var:inspector(Kind,Element) || Element <- Structure].


%% ----------------------------
%% @doc The structures massive verification
-spec mass_verify(List_of_structures,Model) -> boolean()
	when
	Model :: list_of_functions(),
	List_of_structures :: list_of_lists().

mass_verify([],[]) -> true;
mass_verify(_,[]) -> false;
mass_verify([],_) -> false;
mass_verify(Model,List_of_structures) ->
	mass_verify_handler(Model,List_of_structures).


%% ----------------------------
%% @doc The structures massive verification, adjusted return
-spec mass_verify(Return_mode,Model,List_of_structures) ->
	{true,List_of_structures} | boolean()
	when
	Return_mode :: return_list | return_boolean,
	Model :: list_of_functions(),
	List_of_structures :: list_of_lists().

mass_verify(_,[],[]) -> true;
mass_verify(_,[],_) -> false;
mass_verify(_,_,[]) -> false;
mass_verify(return_list,Model,List_of_structures) ->
	case mass_verify_handler(Model,List_of_structures) of
		true -> {true,List_of_structures};
		Verification_result -> Verification_result
	end;
mass_verify(_,Model,List_of_structures) ->
	mass_verify_handler(Model,List_of_structures).


%% ----------------------------
%% @doc The structures massive verification handler
-spec mass_verify_handler(Model,List_of_structures) -> boolean()
	when
	Model :: list_of_functions(),
	List_of_structures :: list_of_lists().

mass_verify_handler(_,[]) -> true;
mass_verify_handler(Model,[Structure|List_of_structures]) ->
	case verify(return_boolean,Model,Structure) of
		true -> mass_verify_handler(Model,List_of_structures);
		Verification_result -> Verification_result
	end.


%% ----------------------------
%% @doc List structure verification
-spec verify(Return_mode,Model,Structure) -> boolean() | {true,Structure}
	when
	Return_mode :: return_structure | return_boolean,
	Model :: list_of_functions(),
	Structure :: list_of_values().

verify(_,[],[]) -> true;
verify(_,[],_) -> false;
verify(_,_,[]) -> false;
verify(Return_mode,Model,Structure) ->
	if
		length(Structure) == length(Model) ->
			case Return_mode of
				return_structure -> verify_structure(Model,Structure);
				_ -> verify_boolean(Model,Structure)
			end;
		true -> false
	end.


%% ----------------------------
%% @doc Structure verification handler, data return mode
-spec verify_structure(Model,Structure) -> {true,Structure} | false
	when
	Model :: list_of_functions(),
	Structure :: list_of_values().

verify_structure(Model,Structure) ->
	case verify_boolean(Model,Structure) of
		true -> {true,Structure};
		Inspection_result -> Inspection_result
	end.


%% ----------------------------
%% @doc Structure verification handler, boolean return mode
-spec verify_boolean(Model,Structure) -> boolean()
	when
	Model :: list_of_functions(),
	Structure :: list_of_values().

verify_boolean([],[]) -> true;
verify_boolean([Inspector|Model],[Element|Structure]) ->
	case Inspector(Element) of
		true -> verify_boolean(Model,Structure);
		Inspection_result -> Inspection_result
	end;
verify_boolean(_,_) -> false.
