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
-export([lpath/3,lpath_run/2]).

%% Module Include Start
-include("../Handler/a.hrl").
%% Module Include End

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