%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Arboreus record based data structures handler
%%%
%%% @end
%%% Created : 06/21/2018 at 21:40
%%%-------------------------------------------------------------------
-module(a_structure_r).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Constants

%% Data types
-include("../data_models/types/types_general.hrl").

%% Data models
-record(test,{one,two,three,four}).
-record(test1,{one,two,three,four}).

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
		"Module (a_structure_r) testing started at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_start), Time_start]
	),
	Record1 = #test{one = 1,two = one,three = 0.1,four = "11"},
	Record2 = #test{one = 2,two = two,three = 0.1,four = "11"},
	Record3 = #test{one = 3,two = three,three = 0.1,four = "22"},
	Record_wrong1 = #test{one = one,two = second_atom,three = 0.2,four = "1234"},
	Record_wrong2 = #test1{one = two,two = second_atom,three = 0.2,four = "1234"},
	Model1 = model(verificator,Record1),
	{name,number,atom,number,list} = model(description,Record1),
	true = verify(return_boolean,Model1,Record1),
	true = verify(return_boolean,Model1,Record2),
	false = verify(return_boolean,Model1,Record_wrong1),
	false = verify(return_boolean,Model1,Record_wrong2),
	io:format("DONE! Fun verify/3 test passed~n"),
	io:format("DONE! Fun model/2 test passed~n"),
	Structures = [Record1,Record2,Record3],
	Structures_wrong = [Record1,Record2,Record_wrong1],
	true = mass_verify(Model1,Structures),
	false = mass_verify(Model1,Structures_wrong),
	false = mass_verify(Model1,[]),
	true = mass_verify([],[]),
	false = mass_verify([],Structures),
	io:format("DONE! Fun mass_verify/2 test passed~n"),
	{true,Structures} = mass_verify(return_list,Model1,Structures),
	io:format("DONE! Fun mass_verify/3 test passed~n"),
	{true,Reference1} = reference(Structures),
	{true,Reference1} = reference(Structures,all),
	{true,Reference1} = reference(Structures,all,[]),
	{true,Reference1} = reference(Structures,[1,2,3,4],[]),
	io:format("DONE! Fun reference/3 test passed: ~p~n",[Reference1]),
	Time_stop = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_structure_r) testing finished at:~n~p (~p)~n",
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

reference(Structures) ->
	case reference_handler(Structures) of
		{true,Reference} ->
			{true,[{Position - 1,Values} || {Position,Values} <- Reference]};
		Result -> Result
	end.


%% ----------------------------
%% @doc Wrapper for reference/3
-spec reference(Structures,Positions) -> false | {true,Reference}
	when
	Structures :: list(),
	Positions :: list() | all,
	Reference :: proplists:proplist().

reference(Structures,Positions) ->
	case reference_handler(Structures,Positions,[]) of
		{true,Reference} ->
			{true,[{Position - 1,Values} || {Position,Values} <- Reference]};
		Result -> Result
	end.


%% ----------------------------
%% @doc Generate reference
-spec reference(Structures,Positions,Reference) ->
	false | {true,Reference}
	when
	Structures :: list(),
	Positions :: list() | all,
	Reference :: proplists:proplist().

reference(Structures,Positions,Reference) ->
	case reference_handler(Structures,Positions,Reference) of
		{true,Reference_out} ->
			{true,[{Position - 1,Values} || {Position,Values} <- Reference_out]};
		Result -> Result
	end.


%% ----------------------------
%% @doc Wrapper for reference_handler/2
-spec reference_handler(Structures) -> false | {true,Reference}
	when
	Structures :: list(),
	Reference :: proplists:proplist().

reference_handler(Structures) -> reference_handler(Structures,all).


%% ----------------------------
%% @doc Wrapper for reference_handler/3
-spec reference_handler(Structures,Positions) -> false | {true,Reference}
	when
	Structures :: list(),
	Positions :: list() | all,
	Reference :: proplists:proplist().

reference_handler(Structures,Positions) ->
	reference_handler(Structures,Positions,[]).


%% ----------------------------
%% @doc Generate reference
-spec reference_handler(Structures,Positions,Reference) ->
	false | {true,Reference}
	when
	Structures :: list(),
	Positions :: list() | all,
	Reference :: proplists:proplist().

reference_handler(Structures,all,Reference) ->
	[Etalon|_] = Structures,
	a_structure_lib:reference(
		?MODULE,Structures,lists:seq(2,tuple_size(Etalon)),Reference
	);
reference_handler(Structures,Positions,Reference) ->
	a_structure_lib:reference(
		?MODULE,Structures,
		[Position + 1 || Position <- Positions],Reference
	).


%% ----------------------------
%% @doc Wrapper for elements/3
-spec elements(Positions,Structure) -> proplists:proplist()
	when
	Positions :: list_of_integers(),
	Structure :: list().

elements(Positions,Structure) ->
	a_structure_t:elements(Positions,Structure).


%% ----------------------------
%% @doc Return data model of the structure
-spec model(Kind,Structure) -> tuple()
	when
	Kind :: verificator | description,
	Structure :: record().

model(Kind,Structure) ->
	if
		tuple_size(Structure) >= 2 ->
			Record_name = element(1,Structure),
			Name_inspector = case Kind of
				description -> name;
				_ ->
					(fun(Name) ->
						case is_atom(Name) of
							true ->
								if
									Record_name == Name -> true;
									true -> false
								end;
							Result -> Result
						end
					end)
			end,
			[_|Structure_data] = tuple_to_list(Structure),
			list_to_tuple(lists:append(
				[Name_inspector],
				a_structure_l:model(Kind,Structure_data)
			));
		true -> false
	end.


%% ----------------------------
%% @doc The structures massive verification
-spec mass_verify(Model,List_of_structures) -> boolean()
	when
	Model :: tuple(),
	List_of_structures :: list_of_records().

mass_verify(Model,List_of_structures) ->
	a_structure_t:mass_verify(Model,List_of_structures).


%% ----------------------------
%% @doc The structures massive verification, adjusted return
-spec mass_verify(Return_mode,Model,List_of_structures) ->
	{true,List_of_structures} | boolean()
	when
	Return_mode :: return_list | return_boolean,
	Model :: tuple(),
	List_of_structures :: list_of_records().

mass_verify(Return_mode,Model,List_of_structures) ->
	a_structure_t:mass_verify(Return_mode,Model,List_of_structures).


%% ----------------------------
%% @doc List structure verification
-spec verify(Return_mode,Model,Structure) -> boolean() | {true,Structure}
	when
	Return_mode :: return_structure | return_boolean,
	Model :: tuple(),
	Structure :: record().

verify(Return_mode,Model,Structure) ->
	a_structure_t:verify(Return_mode,Model,Structure).