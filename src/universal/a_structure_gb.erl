%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Arboreus balanced tree based data structures handler
%%%
%%% @end
%%% Created : 06/23/2018 at 13:33
%%%-------------------------------------------------------------------
-module(a_structure_gb).
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
		"Module (a_structure_gb) testing started at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_start), Time_start]
	),
	Gb_tree1_empty = gb_trees:empty(),
	Gb_tree1_1 = gb_trees:insert(a,1,Gb_tree1_empty),
	Gb_tree1_2 = gb_trees:insert(b,one,Gb_tree1_1),
	Gb_tree1_3 = gb_trees:insert(c,0.1,Gb_tree1_2),
	Gb_tree1 = gb_trees:insert(d,"123",Gb_tree1_3),
	Gb_tree2_empty = gb_trees:empty(),
	Gb_tree2_1 = gb_trees:insert(a,2,Gb_tree2_empty),
	Gb_tree2_2 = gb_trees:insert(b,second_one,Gb_tree2_1),
	Gb_tree2_3 = gb_trees:insert(c,0.2,Gb_tree2_2),
	Gb_tree2 = gb_trees:insert(d,"1234",Gb_tree2_3),
	Gb_tree_wrong_empty = gb_trees:empty(),
	Gb_tree_wrong_1 = gb_trees:insert(a,2,Gb_tree_wrong_empty),
	Gb_tree_wrong_2 = gb_trees:insert(b,0,Gb_tree_wrong_1),
	Gb_tree_wrong_3 = gb_trees:insert(c,0.2,Gb_tree_wrong_2),
	Gb_tree_wrong = gb_trees:insert(d,"1234",Gb_tree_wrong_3),
	Model_desc_empty = gb_trees:empty(),
	Model_desc_1 = gb_trees:insert(a,integer,Model_desc_empty),
	Model_desc_2 = gb_trees:insert(b,atom,Model_desc_1),
	Model_desc_3 = gb_trees:insert(c,float,Model_desc_2),
	Model_desc = gb_trees:insert(d,list,Model_desc_3),
	Model_ver_empty = gb_trees:empty(),
	Model_ver_1 = gb_trees:insert(a,(fun is_integer/1),Model_ver_empty),
	Model_ver_2 = gb_trees:insert(b,(fun is_atom/1),Model_ver_1),
	Model_ver_3 = gb_trees:insert(c,(fun is_float/1),Model_ver_2),
	Model_ver = gb_trees:insert(d,(fun is_list/1),Model_ver_3),
	true = verify(return_boolean,Model_ver,Gb_tree1),
	true = verify(return_boolean,Model_ver,Gb_tree2),
	false = verify(return_boolean,Model_ver,Gb_tree_wrong),
	io:format("DONE! Fun verify/3 test passed~n"),
	Model_description = [Value || {_,Value} <- gb_trees:to_list(Model_desc)],
	Model_description = [Value || {_,Value} <- gb_trees:to_list(model(description,Gb_tree1))],
	Model_test1 = model(verificator,Gb_tree1),
	true = verify(return_boolean,Model_test1,Gb_tree1),
	true = verify(return_boolean,Model_test1,Gb_tree2),
	false = verify(return_boolean,Model_test1,Gb_tree_wrong),
	io:format("DONE! Fun model/2 test passed~n"),
	List_of_structures = [Gb_tree1,Gb_tree2,Gb_tree1],
	List_of_structures_wrong = [Gb_tree1,Gb_tree2,Gb_tree_wrong],
	true = mass_verify(Model_ver,List_of_structures),
	false = mass_verify(Model_ver,List_of_structures_wrong),
	io:format("DONE! Fun mass_verify/2 test passed~n"),
	{true,List_of_structures} = mass_verify(return_list,Model_ver,List_of_structures),
	true = mass_verify(return_boolean,Model_ver,List_of_structures),
	false = mass_verify(return_list,Model_ver,List_of_structures_wrong),
	io:format("DONE! Fun mass_verify/3 test passed~n"),
	Time_stop = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_structure_gb) testing finished at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_stop), Time_stop]
	),
	io:format("Test time is: ~p~n", [Time_stop - Time_start]),
	ok.


%% ----------------------------
%% @doc Return data model of the structure
-spec model(Kind,Structure) -> gb_trees:tree()
	when
	Kind :: verificator | description,
	Structure :: gb_trees:tree().

model(Kind,Structure) ->
	gb_trees:map(
		fun(_,Value) -> a_var:inspector(Kind,Value) end,
		Structure
	).


%% ----------------------------
%% @doc The structures massive verification
-spec mass_verify(Model,List_of_structures) -> boolean()
	when
	Model :: gb_trees:tree(),
	List_of_structures :: list_of_gb_trees().

mass_verify(Model,List_of_structures) ->
	mass_verify(return_boolean,Model,List_of_structures).


%% ----------------------------
%% @doc The structures massive verification
-spec mass_verify(Return_mode,Model,List_of_structures) -> boolean()
	when
	Return_mode :: return_list | return_boolean,
	Model :: gb_trees:tree(),
	List_of_structures :: list_of_gb_trees().

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
	Model :: gb_trees:tree(),
	List_of_structures :: list_of_gb_trees().

mass_verify_handler(_,[]) -> true;
mass_verify_handler(Model,[Structure|List_of_structures]) ->
	case verify(return_boolean,Model,Structure) of
		true -> mass_verify_handler(Model,List_of_structures);
		Verification_result -> Verification_result
	end.


%% ----------------------------
%% @doc List structure verification
-spec verify(Model,Return_mode,Structure) -> boolean() | {true,Structure}
	when
	Model :: gb_trees:tree(),
	Return_mode :: return_structure | return_boolean,
	Structure :: list_of_gb_trees().

verify(Return_mode,Model,Structure) ->
	try
		Model_size = gb_trees:size(Model),
		Structure_size = gb_trees:size(Structure),
		if
			Model_size == Structure_size ->
				Model_keys = gb_trees:keys(Model),
				Structure_keys = gb_trees:keys(Structure),
				if
					Model_keys == Structure_keys ->
						case Return_mode of
							return_structure ->
								verify_structure(Model_keys,Model,Structure);
							_ ->
								verify_boolean(Model_keys,Model,Structure)
						end;
					true -> false
				end;
			true -> false
		end
	catch _:_ -> false end.


%% ----------------------------
%% @doc Structure verification handler, data return mode
-spec verify_structure(Model_keys,Model,Structure) -> {true,Structure} | false
	when
	Model_keys :: list_of_values(),
	Model :: gb_trees:tree(),
	Structure :: gb_trees:tree().

verify_structure(Model_keys,Model,Structure) ->
	case verify_boolean(Model_keys,Model,Structure) of
		true -> {true,Structure};
		Inspection_result -> Inspection_result
	end.


%% ----------------------------
%% @doc Structure verification handler, boolean return mode
-spec verify_boolean(Model_keys,Model,Structure) -> boolean()
	when
	Model_keys :: list_of_values(),
	Model :: gb_trees:tree(),
	Structure :: gb_trees:tree().

verify_boolean([],_,_) -> true;
verify_boolean([Key|Keys],Model_income,Structure_income) ->
	case gb_trees:take(Key,Model_income) of
		{Inspector,Model_out} ->
			case gb_trees:take(Key,Structure_income) of
				{Element,Structure_out} ->
					case Inspector(Element) of
						true -> verify_boolean(Keys,Model_out,Structure_out);
						_ -> false
					end;
				_ -> false
			end;
		_ -> false
	end.