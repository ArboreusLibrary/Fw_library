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
	model/2
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
	Structure1 = {1,atom,0.1,"123"},
	Structure2 = {2,second_atom,0.2,"1234"},
	Structure_wrong = {one,atom,0.1,"123"},
	Model1 = {(fun is_integer/1),(fun is_atom/1),(fun is_float/1),(fun is_list/1)},
	true = verify(return_boolean,Model1,Structure1),
	{true,Structure1} = verify(return_structure,Model1,Structure1),
	false = verify(return_boolean,Model1,Structure_wrong),
	io:format("DONE! Fun verify/3 test passed~n"),
	{integer,atom,float,list} = model(description,Structure1),
	Model2 = model(verificator,Structure1),
	true = verify(return_boolean,Model2,Structure1),
	true = verify(return_boolean,Model2,Structure2),
	false = verify(return_boolean,Model2,Structure_wrong),
	io:format("DONE! Fun model/2 test passed~n"),
	List_of_structures = [Structure1,Structure2,Structure1],
	List_of_structures_wrong = [Structure1,Structure2,Structure_wrong],
	true = mass_verify(Model2,List_of_structures),
	false = mass_verify(Model2,List_of_structures_wrong),
	false = mass_verify(Model1,[]),
	true = mass_verify([],[]),
	false = mass_verify([],List_of_structures),
	io:format("DONE! Fun mass_verify/2 test passed~n"),
	{true,List_of_structures} = mass_verify(return_list,Model2,List_of_structures),
	io:format("DONE! Fun mass_verify/3 test passed~n"),
	Time_stop = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_structure_t) testing finished at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_stop), Time_stop]
	),
	io:format("Test time is: ~p~n", [Time_stop - Time_start]),
	ok.


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