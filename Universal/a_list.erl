%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV (http://alexandr.kirilov.me)
%%% @copyright (C) 2015, Arboreus, (http://arboreus.systems)
%%% @doc
%%%
%%% @end
%%% Created : 21. Jul 2015 21:55
%%%-------------------------------------------------------------------
-module(a_list).
-author("Alexandr KIRILOV (http://alexandr.kirilov.me)").

%% API
-export([lpath/3]).
-export([lpath_verify/1]).
-export([test/1]).

%% Module Include Start
-include("../Handler/a.hrl").
%% Module Include End

-define(TEST_LIST,[
	{key1,[
		{key2,"Value2"},
		{key3,"Value3"}
	]},
	{key4,"Value4"},
	{key5,[
		{key6,[
			{key7,"Value7"},
			{key8,"Value8"},
			{key9,[
				{key10,"Value10"}
			]}
		]},
		{key11,"Value11"},
		{key12,"Value12"}
	]}
]).

-define(TEST_PATH,[key5,key6,key9,key10]).

lpath(add_pair,Path,{Proplist,Pair}) ->
	List = [];
lpath(get_value,Path,Proplist) ->
	Lpath_run = lpath_run(Path,Proplist),
	case Lpath_run of
		{error,_} -> a:error(?FUNCTION_NAME(),m004_001);
		_ -> Lpath_run
	end;
lpath(get_pair,Path,Proplist) ->
	Key = lists:last(Path),
	Lpath_run = lpath_run(Path,Proplist),
	case Lpath_run of
		{error,_} -> a:error(?FUNCTION_NAME(),m004_001);
		_ -> {Key,Lpath_run}
	end.


lpath_run([],Element) -> Element;
lpath_run([Path_point|Path_way],Proplist) ->
	Element = proplists:get_value(Path_point,Proplist),
	case Element of
		undefined -> a:error(?FUNCTION_NAME(),m004_001);
		_ -> lpath_run(Path_way,Element)
	end.

lpath_extract([],_,Result) -> Result;
lpath_extract([Path_point|Path_way],Source,Result) ->
	Source_output = proplists:get_value(Path_point,Source),
	Point_list = proplists:delete(Path_point,Source),
	Result_output = [Point_list|Result],
	lpath_extract(Path_way,Source_output,Result_output).

lpath_insertion() -> [].

lpath_verify(List) when is_list(List) ->
	[]  ;
lpath_verify(_) -> a:error(?FUNCTION_NAME(),a015).


test(Argument) ->
	try
	    list_to_binary(Argument)
	catch
	    error:Error  ->
		    {_,Function} = ?FUNCTION_NAME(),
		    {error,Function,Error}
	end
.