%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Arboreus prop-list based data structures handler
%%%
%%% @end
%%% Created : 06/21/2018 at 21:39
%%%-------------------------------------------------------------------
-module(a_structure_pl).
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
		"Module (a_structure_pl) testing started at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_start), Time_start]
	),
	Proplist1 = [{one,1},{two,one},{three,0.1},{four,"11"}],
	Proplist2 = [{one,2},{two,two},{three,0.1},{four,"11"}],
	Proplist3 = [{one,3},{two,three},{three,0.1},{four,"22"}],
	Proplist_wrong = [{one,one},{two,atom},{three,0.1},{four,"123"}],
	Model1 = [
		{one,(fun is_integer/1)},
		{two,(fun is_atom/1)},
		{three,(fun is_float/1)},
		{four,(fun is_list/1)}
	],
	true = verify(return_boolean,Model1,Proplist1),
	true = verify(return_boolean,Model1,Proplist2),
	{true,Proplist1} = verify(return_structure,Model1,Proplist1),
	false = verify(return_boolean,Model1,Proplist_wrong),
	false = verify(return_boolean,Model1,[]),
	io:format("DONE! Fun verify/3 test passed~n"),
	Model_description = [{one,number},{two,atom},{three,number},{four,list}],
	Model_description = model(description,Proplist1),
	Model_description = model(description,Proplist2),
	Model2 = model(verificator,Proplist1),
	true = verify(return_boolean,Model2,Proplist1),
	true = verify(return_boolean,Model2,Proplist2),
	false = verify(return_boolean,Model2,Proplist_wrong),
	io:format("DONE! Fun model/2 test passed~n"),
	Structures = [Proplist1,Proplist2,Proplist3],
	Structures_wrong = [Proplist1,Proplist2,Proplist_wrong],
	true = mass_verify(Model2,Structures),
	false = mass_verify(Model2,Structures_wrong),
	false = mass_verify(Model1,[]),
	true = mass_verify([],[]),
	false = mass_verify([],Structures),
	io:format("DONE! Fun mass_verify/2 test passed~n"),
	{true,Structures} = mass_verify(return_list,Model2,Structures),
	io:format("DONE! Fun mass_verify/3 test passed~n"),
	{true,Reference1} = reference(Structures),
	{true,Reference1} = reference(Structures,all),
	{true,Reference1} = reference(Structures,all,[]),
	{true,Reference2} = reference(Structures,[one,two,three,four],[]),
	Value_one = [1,2,3],
	Value_one = proplists:get_value(one,Reference2),
	Value_one = proplists:get_value(one,Reference1),
	Value_two = [one,two,three],
	Value_two = proplists:get_value(two,Reference2),
	Value_two = proplists:get_value(two,Reference1),
	Value_three = [0.1],
	Value_three = proplists:get_value(three,Reference2),
	Value_three = proplists:get_value(three,Reference1),
	Value_four = ["11","22"],
	Value_four = proplists:get_value(four,Reference2),
	Value_four = proplists:get_value(four,Reference1),
	io:format("DONE! Fun reference/3 test passed: ~p~n",[Reference1]),
	Time_stop = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_structure_pl) testing finished at:~n~p (~p)~n",
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

reference(Structures,Positions) -> reference(Structures,Positions,[]).


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
	a_structure_lib:reference(
		?MODULE,Structures,proplists:get_keys(Etalon),Reference
	);
reference(Structures,Positions,Reference) ->
	a_structure_lib:reference(
		?MODULE,Structures,Positions,Reference
	).


%% ----------------------------
%% @doc Wrapper for elements/3
-spec elements(Positions,Structure) -> proplists:proplist()
	when
	Positions :: list_of_integers(),
	Structure :: list().

elements(Positions,Structure) -> elements(Positions,Structure,[]).


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
			{Position,proplists:get_value(Position,Structure)}
		])
	).


%% ----------------------------
%% @doc Return data model of the structure
-spec model(Kind,Structure) -> list_of_functions() | list_of_atoms()
	when
	Kind :: verificator | description,
	Structure :: list_of_values().

model(Kind,Structure) ->
	[{Name,a_var:inspector(Kind,Element)} || {Name,Element} <- Structure].


%% ----------------------------
%% @doc The structures massive verification
-spec mass_verify(Model,List_of_structures) -> boolean()
	when
	Model :: proplists:proplist(),
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
	Model :: proplists:proplist(),
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
	Model :: proplists:proplist(),
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
	Model :: proplists:proplist(),
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
	Model :: proplists:proplist(),
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
	Model :: proplists:proplist(),
	Structure :: list_of_values().

verify_boolean([],[]) -> true;
verify_boolean(Model,[{Name,Element}|Structure]) ->
	case proplists:get_value(Name,Model) of
		undefined -> false;
		Inspector ->
			case Inspector(Element) of
				true -> verify_boolean(proplists:delete(Name,Model),Structure);
				Inspection_result -> Inspection_result
			end
	end;
verify_boolean(_,_) -> false.