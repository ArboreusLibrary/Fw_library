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
-vsn("0.0.4.144").

%% Module API
-export([last_modified/1]).
-export([expires/1]).
-export([cache/1]).

%% System include

%% Module Include Start
-include("../Handler/a.hrl").
%% Module Include End

%%-----------------------------------
%% @spec last_modified(Time_in) -> list()
%% where
%%      Time_in() :: integer() | tuple() | current
%% @doc Return a list() within HTTP Header formated for Yaws out() function.
%% Example: "Last-Modified: Fri, 30 Oct 1998 14:19:41 GMT"
-spec last_modified(Time_in) -> list()
	when Time_in :: pos_integer() | tuple() | current.

last_modified(Time_in) when is_integer(Time_in), Time_in > 0 ->
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

%%-----------------------------------
%% @spec expires(Time_in) -> list().
%% where
%%      Time_in :: integer() | tuple() | current
%% @doc Return a list() within HTTP Header formated for Yaws out() function.
%% Example: "Expires: Fri, 30 Oct 1998 14:19:41 GMT"
-spec expires(Time_in) -> list()
	when Time_in :: pos_integer() | tuple() | current.

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

%%-----------------------------------
%% @spec cache(Operation) -> list()
%% where
%%      Operation() = no
%% @doc Return a list() within HTTP headers fromated for Yaws out() function.
-spec cache(Operation) -> list()
	when Operation :: no.

cache(no) ->
	[
		{header,"Cache-Control: no-cache, no-store, must-revalidate"},
		{header,"Pragma: no-cache"},
		expires(1),
		last_modified(current)
	];
cache(_) -> a:error(?FUNCTION_NAME(),a000).