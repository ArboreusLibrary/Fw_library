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
	model/2
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
	List1 = [1,atom,0.1,"123"],
	List2 = [2,second_atom,0.2,"1234"],
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
	List_of_structures = [List1,List2,List1],
	List_of_structures_wrong = [List1,List2,List_wrong],
	true = mass_verify(Model1,List_of_structures),
	true = mass_verify(Model2,List_of_structures),
	false = mass_verify(Model1,List_of_structures_wrong),
	false = mass_verify(Model2,List_of_structures_wrong),
	false = mass_verify(Model1,[]),
	true = mass_verify([],[]),
	false = mass_verify([],List_of_structures),
	io:format("DONE! Fun mass_verify/2 test passed~n"),
	{true,List_of_structures} = mass_verify(return_list,Model1,List_of_structures),
	{true,List_of_structures} = mass_verify(return_list,Model2,List_of_structures),
	io:format("DONE! Fun mass_verify/3 test passed~n"),
	Time_stop = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_structure_l) testing finished at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_stop), Time_stop]
	),
	io:format("Test time is: ~p~n", [Time_stop - Time_start]),
	ok.


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
