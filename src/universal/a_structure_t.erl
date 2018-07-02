%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Arboreus tuple based data structures handler
%%%
%%% @end
%%% Created : 06/21/2018 at 21:38
%%%-------------------------------------------------------------------
-module(a_structure_t).
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
		"Module (a_structure_t) testing started at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_start), Time_start]
	),
	
	Tuple1 = {1,one,0.1,"11"},
	Tuple2 = {2,two,0.1,"22"},
	Tuple3 = {3,three,0.1,"11"},
	Tuple_wrong = {one,atom,0.1,"123"},
	Model1 = {(fun is_integer/1),(fun is_atom/1),(fun is_float/1),(fun is_list/1)},
	true = verify(return_boolean,Model1,Tuple1),
	{true,Tuple1} = verify(return_structure,Model1,Tuple1),
	false = verify(return_boolean,Model1,Tuple_wrong),
	io:format("DONE! Fun verify/3 test passed~n"),
	{number,atom,number,list} = model(description,Tuple1),
	Model2 = model(verificator,Tuple1),
	true = verify(return_boolean,Model2,Tuple1),
	true = verify(return_boolean,Model2,Tuple2),
	false = verify(return_boolean,Model2,Tuple_wrong),
	io:format("DONE! Fun model/2 test passed~n"),
	Structures = [Tuple1,Tuple2,Tuple3],
	Structures_wrong = [Tuple1,Tuple2,Tuple_wrong],
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
	{true,Reference1} = reference(Structures,[1,2,3,4],[]),
	io:format("DONE! Fun reference/3 test passed: ~p~n",[Reference1]),
	Time_stop = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_structure_t) testing finished at:~n~p (~p)~n",
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
		?MODULE,Structures,{all,tuple_size(Etalon)},Reference
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
			{Position,element(Position,Structure)}
		])
	).


%% ----------------------------
%% @doc Return data model of the structure
-spec model(Kind,Structure) -> tuple()
	when
	Kind :: verificator | description,
	Structure :: tuple().

model(Kind,Structure) ->
	list_to_tuple(
		a_structure_l:model(Kind,tuple_to_list(Structure))
	).


%% ----------------------------
%% @doc The structures massive verification
-spec mass_verify(List_of_structures,Model) -> boolean()
	when
	List_of_structures :: list_of_tuples(),
	Model :: tuple().

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
	Model :: tuple(),
	List_of_structures :: list_of_tuples().

mass_verify(_,[],[]) -> true;
mass_verify(_,[],_) -> false;
mass_verify(_,_,[]) -> false;
mass_verify(return_list,Model,List_of_structures) ->
	case mass_verify_handler(Model,List_of_structures) of
		true -> {true,List_of_structures};
		Verification_result -> Verification_result
	end;
mass_verify(Model,_,List_of_structures) ->
	mass_verify_handler(Model,List_of_structures).


%% ----------------------------
%% @doc The structures massive verification handler
-spec mass_verify_handler(Model,List_of_structures) -> boolean()
	when
	Model :: tuple(),
	List_of_structures :: list_of_tuples().

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
	Model :: tuple(),
	Structure :: tuple().

verify(_,[],[]) -> true;
verify(_,[],_) -> false;
verify(_,_,[]) -> false;
verify(Return_mode,Model,Structure) ->
	if
		tuple_size(Structure) == tuple_size(Model) ->
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
	Model :: tuple(),
	Structure :: tuple().

verify_structure(Model,Structure) ->
	case verify_boolean(Model,Structure) of
		true -> {true,Structure};
		Inspection_result -> Inspection_result
	end.


%% ----------------------------
%% @doc Structure verification handler, boolean return mode
-spec verify_boolean(Model,Structure) -> boolean()
	when
	Model :: tuple(),
	Structure :: tuple().

verify_boolean(Model,Structure) ->
	verify_boolean(tuple_size(Model),Model,Structure).


%% ----------------------------
%% @doc Structure verification handler, boolean return mode
-spec verify_boolean(Counter,Model,Structure) -> boolean()
	when
	Counter :: pos_integer(),
	Model :: tuple(),
	Structure :: tuple().

verify_boolean(0,_,_) -> true;
verify_boolean(Counter,Model,Structure) ->
	Inspector = element(Counter,Model),
	case Inspector(element(Counter,Structure)) of
		true -> verify_boolean(Counter - 1,Model,Structure);
		_ -> false
	end.