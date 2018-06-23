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
	Proplist1 = [{one,1},{two,atom},{three,0.1},{four,"123"}],
	Proplist2 = [{one,2},{two,second_atom},{three,0.2},{four,"1234"}],
	Proplist_wrong = [{one,one},{two,atom},{three,0.1},{four,"123"}],
	Model1 = [
		{one,(fun is_integer/1)},
		{two,(fun is_atom/1)},
		{three,(fun is_float/1)},
		{four,(fun is_list/1)}
	],
	true = verify(Model1,return_boolean,Proplist1),
	true = verify(Model1,return_boolean,Proplist2),
	{true,Proplist1} = verify(Model1,return_structure,Proplist1),
	false = verify(Model1,return_boolean,Proplist_wrong),
	false = verify(Model1,return_boolean,[]),
	io:format("DONE! Fun verify/3 test passed~n"),
	Model_description = [{one,integer},{two,atom},{three,float},{four,list}],
	Model_description = model(description,Proplist1),
	Model_description = model(description,Proplist2),
	Model2 = model(verificator,Proplist1),
	true = verify(Model2,return_boolean,Proplist1),
	true = verify(Model2,return_boolean,Proplist2),
	false = verify(Model2,return_boolean,Proplist_wrong),
	io:format("DONE! Fun model/2 test passed~n"),
	List_of_structures = [Proplist1,Proplist2,Proplist1],
	List_of_structures_wrong = [Proplist1,Proplist2,Proplist_wrong],
	true = mass_verify(Model2,List_of_structures),
	false = mass_verify(Model2,List_of_structures_wrong),
	false = mass_verify(Model1,[]),
	true = mass_verify([],[]),
	false = mass_verify([],List_of_structures),
	io:format("DONE! Fun mass_verify/2 test passed~n"),
	{true,List_of_structures} = mass_verify(Model2,return_list,List_of_structures),
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
-spec mass_verify(Model,Return_mode,List_of_structures) ->
	{true,List_of_structures} | boolean()
	when
	Model :: proplists:proplist(),
	Return_mode :: return_list | return_boolean,
	List_of_structures :: list_of_lists().

mass_verify([],_,[]) -> true;
mass_verify([],_,_) -> false;
mass_verify(_,_,[]) -> false;
mass_verify(Model,return_list,List_of_structures) ->
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
	Model :: proplists:proplist(),
	List_of_structures :: list_of_lists().

mass_verify_handler(_,[]) -> true;
mass_verify_handler(Model,[Structure|List_of_structures]) ->
	case verify(Model,return_boolean,Structure) of
		true -> mass_verify_handler(Model,List_of_structures);
		Verification_result -> Verification_result
	end.


%% ----------------------------
%% @doc List structure verification
-spec verify(Model,Return_mode,Structure) -> boolean() | {true,Structure}
	when
	Model :: proplists:proplist(),
	Return_mode :: return_structure | return_boolean,
	Structure :: list_of_values().

verify([],_,[]) -> true;
verify([],_,_) -> false;
verify(_,_,[]) -> false;
verify(Model,Return_mode,Structure) ->
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