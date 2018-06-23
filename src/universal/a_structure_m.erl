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
	true = verify(Model1,return_boolean,Map1),
	true = verify(Model1,return_boolean,Map2),
	false = verify(Model1,return_boolean,Map_wrong),
	io:format("DONE! Fun verify/3 test passed~n"),
	Model2 = model(verificator,Map1),
	Model_description = #{a => integer,b => atom,c => float,d => list},
	Model_description = model(description,Map1),
	true = verify(Model2,return_boolean,Map2),
	false = verify(Model2,return_boolean,Map_wrong),
	io:format("DONE! Fun model/2 test passed~n"),
	List_of_structures = [Map1,Map2,Map1],
	List_of_structures_wrong = [Map1,Map2,Map_wrong],
	true = mass_verify(Model1,List_of_structures),
	false = mass_verify(Model1,List_of_structures_wrong),
	io:format("DONE! Fun mass_verify/2 test passed~n"),
	{true,List_of_structures} = mass_verify(Model1,return_list,List_of_structures),
	{true,List_of_structures} = mass_verify(Model2,return_list,List_of_structures),
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
	model_handler(Kind,#{},maps:iterator(Structure)).


%% ----------------------------
%% @doc Handler for map/2
-spec model_handler(Kind,Model,Structure) -> map()
	when
	Kind :: verificator | description,
	Model :: map(),
	Structure :: map().

model_handler(_,Model,none) -> Model;
model_handler(Kind,Model,Structure) ->
	{Name,Element,Structure_next} = maps:next(Structure),
	model_handler(
		Kind,
		maps:put(Name,a_var:inspector(Kind,Element),Model),
		Structure_next
	).


%% ----------------------------
%% @doc The structures massive verification
-spec mass_verify(Model,List_of_structures) -> boolean()
	when
	Model :: map(),
	List_of_structures :: list_of_maps().

mass_verify(Model,List_of_structures) ->
	mass_verify_handler(Model,List_of_structures).


%% ----------------------------
%% @doc The structures massive verification, adjusted return
-spec mass_verify(Model,Return_mode,List_of_structures) ->
	{true,List_of_structures} | boolean()
	when
	Model :: map(),
	Return_mode :: return_list | return_boolean,
	List_of_structures :: list_of_maps().

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
	Model :: map(),
	List_of_structures :: list_of_maps().

mass_verify_handler(_,[]) -> true;
mass_verify_handler(Model,[Structure|List_of_structures]) ->
	case verify(Model,return_boolean,Structure) of
		true -> mass_verify_handler(Model,List_of_structures);
		Verification_result -> Verification_result
	end.


%% ----------------------------
%% @doc Structure verification
-spec verify(Model,Return_mode,Structure) -> boolean() | {true,Structure}
	when
	Model :: map(),
	Return_mode :: return_structure | return_boolean,
	Structure :: map().

verify(Model,Return_mode,Structure) ->
	if
		map_size(Structure) == map_size(Model) ->
			try
				case Return_mode of
					return_structure -> verify_structure(maps:iterator(Model),Structure);
					_ -> verify_boolean(maps:iterator(Model),Structure)
				end
			catch _:_ -> false end;
		true -> false
	end.


%% ----------------------------
%% @doc Structure verification handler, data return mode
-spec verify_structure(Model,Structure) -> {true,Structure} | false
	when
	Model :: map(),
	Structure :: map().

verify_structure(Model,Structure) ->
	case verify_boolean(Model,Structure) of
		true -> {true,Structure};
		Verification_result -> Verification_result
	end.


%% ----------------------------
%% @doc Structure verification handler, boolean return mode
-spec verify_boolean(Model,Structure) -> boolean()
	when
	Model :: map(),
	Structure :: map().

verify_boolean(none,_) -> true;
verify_boolean(Model,Structure) ->
	{Name,Inspector,Next_model} = maps:next(Model),
	case Inspector(maps:get(Name,Structure)) of
		true -> verify_boolean(Next_model,Structure);
		_ -> false
	end.