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
	model/2
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
	Proplist_diverse = [{one,1},{two,atom},{three,0.1},{four,"123"}],
	Proplist_diverse_wrong = [{one,one},{two,atom},{three,0.1},{four,"123"}],
	Model1 = [
		{one,(fun is_integer/1)},
		{two,(fun is_atom/1)},
		{three,(fun is_float/1)},
		{four,(fun is_list/1)}
	],
	true = verify(Proplist_diverse,Model1,return_boolean),
	{true,Proplist_diverse} = verify(Proplist_diverse,Model1,return_structure),
	false = verify(Proplist_diverse_wrong,Model1,return_boolean),
	false = verify([],Model1,return_boolean),
	io:format("DONE! Fun verify/3 test passed~n"),
	Model_description = [{one,integer},{two,atom},{three,float},{four,list}],
	Model_description = model(description,Proplist_diverse),
	Model2 = model(verificator,Proplist_diverse),
	true = verify(Proplist_diverse,Model2,return_boolean),
	false = verify(Proplist_diverse_wrong,Model2,return_boolean),
	io:format("DONE! Fun model/2 test passed~n"),
	List_of_structures = [Proplist_diverse,Proplist_diverse,Proplist_diverse],
	List_of_structures_wrong = [Proplist_diverse,Proplist_diverse,Proplist_diverse_wrong],
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
		"Module (a_structure_pl) testing finished at:~n~p (~p)~n",
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
	[{Name,a_var:inspector(Kind,Element)} || {Name,Element} <- Structure].


%% ----------------------------
%% @doc The structures massive verification
-spec mass_verify(List_of_structures,Model) -> boolean()
	when
	List_of_structures :: list_of_lists(),
	Model :: list_of_functions().

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
	List_of_structures :: list_of_lists(),
	Model :: list_of_functions(),
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
	List_of_structures :: list_of_lists(),
	Model :: list_of_functions().

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
	Structure :: list_of_values(),
	Model :: list_of_functions(),
	Return_mode :: return_structure | return_boolean.

verify([],[],_) -> true;
verify(_,[],_) -> false;
verify([],_,_) -> false;
verify(Structure,Model,Return_mode) ->
	if
		length(Structure) == length(Model) ->
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
	Structure :: list_of_values(),
	Model :: list_of_functions().

verify_structure(Structure,Model) ->
	case verify_boolean(Structure,Model) of
		true -> {true,Structure};
		Inspection_result -> Inspection_result
	end.


%% ----------------------------
%% @doc Structure verification handler, boolean return mode
-spec verify_boolean(Structure,Model) -> boolean()
	when
	Structure :: list_of_values(),
	Model :: list_of_functions().

verify_boolean([],[]) -> true;
verify_boolean([{Name,Element}|Structure],Model) ->
	case proplists:get_value(Name,Model) of
		undefined -> false;
		Inspector ->
			case Inspector(Element) of
				true -> verify_boolean(Structure,proplists:delete(Name,Model));
				Inspection_result -> Inspection_result
			end
	end;
verify_boolean(_,_) -> false.