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
	true = verify(Structure1,Model1,return_boolean),
	{true,Structure1} = verify(Structure1,Model1,return_structure),
	false = verify(Structure_wrong,Model1,return_boolean),
	io:format("DONE! Fun verify/3 test passed~n"),
	{integer,atom,float,list} = model(description,Structure1),
	Model2 = model(verificator,Structure1),
	true = verify(Structure1,Model2,return_boolean),
	true = verify(Structure2,Model2,return_boolean),
	false = verify(Structure_wrong,Model2,return_boolean),
	io:format("DONE! Fun model/2 test passed~n"),
	List_of_structures = [Structure1,Structure2,Structure1],
	List_of_structures_wrong = [Structure1,Structure2,Structure_wrong],
	true = mass_verify(List_of_structures,Model2),
	false = mass_verify(List_of_structures_wrong,Model2),
	false = mass_verify([],Model1),
	true = mass_verify([],[]),
	false = mass_verify(List_of_structures,[]),
	io:format("DONE! Fun mass_verify/2 test passed~n"),
	{true,List_of_structures} = mass_verify(List_of_structures,Model2,return_list),
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
mass_verify(List_of_structures,Model) ->
	mass_verify_handler(List_of_structures,Model).


%% ----------------------------
%% @doc The structures massive verification, adjusted return
-spec mass_verify(List_of_structures,Model,Return_mode) ->
	{true,List_of_structures} | boolean()
	when
	List_of_structures :: list_of_tuples(),
	Model :: tuple(),
	Return_mode :: return_list | return_boolean.

mass_verify([],[],_) -> true;
mass_verify(_,[],_) -> false;
mass_verify([],_,_) -> false;
mass_verify(List_of_structures,Model,return_list) ->
	case mass_verify_handler(List_of_structures,Model) of
		true -> {true,List_of_structures};
		Verification_result -> Verification_result
	end;
mass_verify(List_of_structures,Model,_) ->
	mass_verify_handler(List_of_structures,Model).


%% ----------------------------
%% @doc The structures massive verification handler
-spec mass_verify_handler(List_of_structures,Model) -> boolean()
	when
	List_of_structures :: list_of_tuples(),
	Model :: tuple().

mass_verify_handler([],_) -> true;
mass_verify_handler([Structure|List_of_structures],Model) ->
	case verify(Structure,Model,return_boolean) of
		true -> mass_verify_handler(List_of_structures,Model);
		Verification_result -> Verification_result
	end.


%% ----------------------------
%% @doc List structure verification
-spec verify(Structure,Model,Return_mode) -> boolean() | {true,Structure}
	when
	Structure :: tuple(),
	Model :: tuple(),
	Return_mode :: return_structure | return_boolean.

verify([],[],_) -> true;
verify(_,[],_) -> false;
verify([],_,_) -> false;
verify(Structure,Model,Return_mode) ->
	if
		tuple_size(Structure) == tuple_size(Model) ->
			case Return_mode of
				return_structure -> verify_structure(Structure,Model);
				_ -> verify_boolean(Structure,Model)
			end;
		true -> false
	end.


%% ----------------------------
%% @doc Structure verification handler, data return mode
-spec verify_structure(Structure,Model) -> {true,Structure} | false
	when
	Structure :: tuple(),
	Model :: tuple().

verify_structure(Structure,Model) ->
	case verify_boolean(Structure,Model) of
		true -> {true,Structure};
		Inspection_result -> Inspection_result
	end.


%% ----------------------------
%% @doc Structure verification handler, boolean return mode
-spec verify_boolean(Structure,Model) -> boolean()
	when
	Structure :: tuple(),
	Model :: tuple().

verify_boolean(Structure,Model) ->
	verify_boolean(tuple_size(Model),Structure,Model).


%% ----------------------------
%% @doc Structure verification handler, boolean return mode
-spec verify_boolean(Counter,Structure,Model) -> boolean()
	when
	Counter :: pos_integer(),
	Structure :: tuple(),
	Model :: tuple().

verify_boolean(0,_,_) -> true;
verify_boolean(Counter,Structure,Model) ->
	Inspector = element(Counter,Model),
	case Inspector(element(Counter,Structure)) of
		true -> verify_boolean(Counter - 1,Structure,Model);
		_ -> false
	end.