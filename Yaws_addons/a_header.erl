%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV (http://alexandr.kirilov.me)
%%% @copyright (C) 2015, Arboreus, (http://arboreus.systems)
%%% @doc
%%%
%%% @end
%%% Created : 08. Aug 2015 18:48
%%%-------------------------------------------------------------------
-module(a_header).
-author("Alexandr KIRILOV (http://alexandr.kirilov.me)").
-vsn("0.0.3.98").

%% Module API
-export([last_modified/1]).
-export([expires/1]).
-export([cache/1]).
-export([error/2]).

%% System include

%% Module Include Start
-include("../Handler/a.hrl").
%% Module Include End

%% @spec last_modified(Time_in()) -> list()
%% where
%%      Time_in() = integer() | tuple() | current
%% @doc Return a list() within HTTP Header formated for Yaws out() function.
%% Example: "Last-Modified: Fri, 30 Oct 1998 14:19:41 GMT"
last_modified(Time_in) when is_integer(Time_in) == true, Time_in > 0 ->
	Time = a:str(a_time:format(rfc822,{timestamp,a_time:timestamp_to_tuple(Time_in)})),
	[{header,["Last-Modified:",Time]}];
last_modified({{Year,Month,Day},{Hour,Minute,Second}})
	when
		is_integer(Year) == true, Year > 0,
		is_integer(Month) == true, Month > 0, Month =< 12,
		is_integer(Day) == true, Day > 0, Day =< 31,
		is_integer(Hour) == true, Hour >= 0, Hour =< 23,
		is_integer(Minute) == true, Minute >= 0, Minute =< 59,
		is_integer(Second) == true, Second >= 0, Second =< 59 ->
	Time = a:str(a_time:format(rfc822,{date,{{Year,Month,Day},{Hour,Minute,Second}}})),
	[{header,["Last-Modified:",Time]}];
last_modified(current) -> last_modified(erlang:localtime());
last_modified(_) -> a:error(?FUNCTION_NAME(),a000).

%% @spec expires(Time_in()) -> list()
%% where
%%      Time_in() = integer() | tuple() | current
%% @doc Return a list() within HTTP Header formated for Yaws out() function.
%% Example: "Expires: Fri, 30 Oct 1998 14:19:41 GMT"
expires(Time_in) when is_integer(Time_in) == true, Time_in > 0 ->
	Time = a:str(a_time:format(rfc822,{timestamp,a_time:timestamp_to_tuple(Time_in)})),
	[{header,["Expires:",Time]}];
expires({{Year,Month,Day},{Hour,Minute,Second}})
	when
		is_integer(Year) == true, Year > 0,
		is_integer(Month) == true, Month > 0, Month =< 12,
		is_integer(Day) == true, Day > 0, Day =< 31,
		is_integer(Hour) == true, Hour >= 0, Hour =< 23,
		is_integer(Minute) == true, Minute >= 0, Minute =< 59,
		is_integer(Second) == true, Second >= 0, Second =< 59 ->
	Time = a:str(a_time:format(rfc822,{date,{{Year,Month,Day},{Hour,Minute,Second}}})),
	[{header,["Expires:",Time]}];
expires(current) -> expires(erlang:localtime());
expires(_) -> a:error(?FUNCTION_NAME(),a000).

%% @spec cache(Operation()) -> list()
%% where
%%      Operation() = atom() | tuple()
%% @doc Return a list() within HTTP headers fromated for Yaws out() function.
cache(no) ->
	[
		{header,"Cache-Control: no-cache, no-store, must-revalidate"},
		{header,"Pragma: no-cache"},
		expires(1),
		last_modified(current)
	];
cache(_) -> a:error(?FUNCTION_NAME(),a000).

%% @spec error(Code(),Debug()) -> list()
%% where
%%      Code() = atom(),
%%      Debug() = true | false
%% @doc Return a list() within HTTP headers supposed for Application Error identification
error(Code,false) when is_atom(Code) == true ->
	[
		{header,["Application:","error"]},
		{header,["Error:",atom_to_list(Code)]}
	];
error(Code,true) when is_atom(Code) == true ->
	{error,{Module,Function,Arity,Description}} = a:error(?FUNCTION_NAME(),Code),
	[
		a_header:error(Code,false),
		{header,["A_module:",atom_to_list(Module)]},
		{header,["A_function:",atom_to_list(Function)]},
		{header,["A_arity:",integer_to_list(Arity)]},
		{header,["A_description:",Description]}
	];
error(_,_) -> a:error(?FUNCTION_NAME(),a000).