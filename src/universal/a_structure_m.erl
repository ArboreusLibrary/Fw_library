%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Arboreus map based data structures handler
%%%
%%% @end
%%% Created : 06/21/2018 at 21:41
%%%-------------------------------------------------------------------
-module(a_structure_m).
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
		"Module (a_structure_m) testing started at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_start), Time_start]
	),
	Map1 = #{a => 1,b => atom,c => 0.1,d => "123"},
	Map2 = #{a => 2,b => second_atom,c => 0.1,d => "123"},
	Map_wrong = #{a => a,b => atom,c => 0.1,d => "123"},
	Model1 = #{
		a => (fun is_integer/1),
		b => (fun is_atom/1),
		c => (fun is_float/1),
		d => (fun is_list/1)
	},
	true = verify(Map1,Model1,return_boolean),
	true = verify(Map2,Model1,return_boolean),
	false = verify(Map_wrong,Model1,return_boolean),
	io:format("DONE! Fun verify/3 test passed~n"),
	Model2 = model(verificator,Map1),
	Model_description = #{a => integer,b => atom,c => float,d => list},
	Model_description = model(description,Map1),
	true = verify(Map2,Model2,return_boolean),
	false = verify(Map_wrong,Model2,return_boolean),
	io:format("DONE! Fun model/2 test passed~n"),
	List_of_structures = [Map1,Map2,Map1],
	List_of_structures_wrong = [Map1,Map2,Map_wrong],
	true = mass_verify(List_of_structures,Model1),
	false = mass_verify(List_of_structures_wrong,Model1),
	io:format("DONE! Fun mass_verify/2 test passed~n"),
	{true,List_of_structures} = mass_verify(List_of_structures,Model1,return_list),
	{true,List_of_structures} = mass_verify(List_of_structures,Model2,return_list),
	io:format("DONE! Fun mass_verify/3 test passed~n"),
	Time_stop = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_structure_m) testing finished at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_stop), Time_stop]
	),
	io:format("Test time is: ~p~n", [Time_stop - Time_start]),
	ok.


%% ----------------------------
%% @doc Return data model of the structure
-spec model(Kind,Structure) -> tuple()
	when
	Kind :: verificator | description,
	Structure :: map().

model(Kind,Structure) ->
	model_handler(Kind,maps:iterator(Structure),#{}).


%% ----------------------------
%% @doc Handler for map/2
-spec model_handler(Kind,Structure,Model) -> map()
	when
	Kind :: verificator | description,
	Structure :: map(),
	Model :: map().

model_handler(_,none,Model) -> Model;
model_handler(Kind,Structure,Model) ->
	{Name,Element,Structure_next} = maps:next(Structure),
	model_handler(
		Kind,Structure_next,
		maps:put(Name,a_var:inspector(Kind,Element),Model)
	).


%% ----------------------------
%% @doc The structures massive verification
-spec mass_verify(List_of_structures,Model) -> boolean()
	when
	List_of_structures :: list_of_maps(),
	Model :: map().

mass_verify(List_of_structures,Model) ->
	mass_verify_handler(List_of_structures,Model).


%% ----------------------------
%% @doc The structures massive verification, adjusted return
-spec mass_verify(List_of_structures,Model,Return_mode) ->
	{true,List_of_structures} | boolean()
	when
	List_of_structures :: list_of_maps(),
	Model :: map(),
	Return_mode :: return_list | return_boolean.

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
	List_of_structures :: list_of_maps(),
	Model :: map().

mass_verify_handler([],_) -> true;
mass_verify_handler([Structure|List_of_structures],Model) ->
	case verify(Structure,Model,return_boolean) of
		true -> mass_verify_handler(List_of_structures,Model);
		Verification_result -> Verification_result
	end.


%% ----------------------------
%% @doc Structure verification
-spec verify(Structure,Model,Return_mode) -> boolean() | {true,Structure}
	when
	Structure :: map(),
	Model :: map(),
	Return_mode :: return_structure | return_boolean.

verify(Structure,Model,Return_mode) ->
	if
		map_size(Structure) == map_size(Model) ->
			try
				case Return_mode of
					return_structure ->
						verify_structure(Structure,maps:iterator(Model));
					_ ->
						verify_boolean(Structure,maps:iterator(Model))
				end
			catch
				_:_ -> false
			end;
		true -> false
	end.


%% ----------------------------
%% @doc Structure verification handler, data return mode
-spec verify_structure(Structure,Model) -> {true,Structure} | false
	when
	Structure :: map(),
	Model :: map().

verify_structure(Structure,Model) ->
	case verify_boolean(Structure,Model) of
		true -> {true,Structure};
		Verification_result -> Verification_result
	end.


%% ----------------------------
%% @doc Structure verification handler, boolean return mode
-spec verify_boolean(Structure,Model) -> boolean()
	when
	Structure :: map(),
	Model :: map().

verify_boolean(_,none) -> true;
verify_boolean(Structure,Model) ->
	{Name,Inspector,Next_model} = maps:next(Model),
	case Inspector(maps:get(Name,Structure)) of
		true -> verify_boolean(Structure,Next_model);
		_ -> false
	end.