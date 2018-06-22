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
		"Module (a_structure_r) testing started at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_start), Time_start]
	),
	Record1 = #test{one = 1,two = atom,three = 0.1,four = "123"},
	Record2 = #test{one = 2,two = second_atom,three = 0.2,four = "1234"},
	Record_wrong = #test{one = one,two = second_atom,three = 0.2,four = "1234"},
	Model1 = model(verificator,Record1),
	{name,integer,atom,float,list} = model(description,Record1),
	true = verify(Record1,Model1,return_boolean),
	true = verify(Record2,Model1,return_boolean),
	false = verify(Record_wrong,Model1,return_boolean),
	io:format("DONE! Fun verify/3 test passed~n"),
	io:format("DONE! Fun model/2 test passed~n"),
	List_of_structures = [Record1,Record2,Record1],
	List_of_structures_wrong = [Record1,Record2,Record_wrong],
	true = mass_verify(List_of_structures,Model1),
	false = mass_verify(List_of_structures_wrong,Model1),
	false = mass_verify([],Model1),
	true = mass_verify([],[]),
	false = mass_verify(List_of_structures,[]),
	io:format("DONE! Fun mass_verify/2 test passed~n"),
	{true,List_of_structures} = mass_verify(List_of_structures,Model1,return_list),
	io:format("DONE! Fun mass_verify/3 test passed~n"),
	Time_stop = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_structure_r) testing finished at:~n~p (~p)~n",
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
-spec mass_verify(List_of_structures,Model) -> boolean()
	when
	List_of_structures :: list_of_tuples(),
	Model :: tuple().

mass_verify(List_of_structures,Model) ->
	a_structure_t:mass_verify(List_of_structures,Model).


%% ----------------------------
%% @doc The structures massive verification, adjusted return
-spec mass_verify(List_of_structures,Model,Return_mode) ->
	{true,List_of_structures} | boolean()
	when
	List_of_structures :: list_of_tuples(),
	Model :: tuple(),
	Return_mode :: return_list | return_boolean.

mass_verify(List_of_structures,Model,Return_mode) ->
	a_structure_t:mass_verify(List_of_structures,Model,Return_mode).


%% ----------------------------
%% @doc List structure verification
-spec verify(Structure,Model,Return_mode) -> boolean() | {true,Structure}
	when
	Structure :: tuple(),
	Model :: tuple(),
	Return_mode :: return_structure | return_boolean.

verify(Structure,Model,Return_mode) ->
	a_structure_t:verify(Structure,Model,Return_mode).