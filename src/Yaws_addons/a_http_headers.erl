%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV (http://alexandr.kirilov.me)
%%% @copyright (C) 2015, Arboreus, (http://arboreus.systems)
%%% @doc
%%%
%%% @end
%%% Created : 08. Aug 2015 18:48
%%%-------------------------------------------------------------------
-module(a_http_headers).
-author("Alexandr KIRILOV (http://alexandr.kirilov.me)").
-vsn("0.0.4.144").

%% Module API
-export([
	last_modified/1,
	expires/1,
	cache/1,
	json/1,
	csv/2,
	xml/1
]).

%% System include

%% Module Include Start
-include("../Handler/a.hrl").
%% Module Include End

%%-----------------------------------
%% @spec last_modified(Time_in) -> list() | {error,_Reason}.
%% where
%%      Time_in() :: integer() | tuple() | current
%% @doc Return a list() within HTTP Header formated for Yaws out() function.
%% Example: "Last-Modified: Fri, 30 Oct 1998 14:19:41 GMT"
-spec last_modified(Time_in) -> list() | {error,_Reason}
	when Time_in :: pos_integer() | tuple() | current.

last_modified(Time_in) when is_integer(Time_in), Time_in > 0 ->
	Time = a:to_string(a_time:format(rfc822,{timestamp,a_time:timestamp_to_tuple(Time_in)})),
	[{header,["Last-Modified:",Time]}];
last_modified({{Year,Month,Day},{Hour,Minute,Second}})
	when
		is_integer(Year) == true, Year > 0,
		is_integer(Month) == true, Month > 0, Month =< 12,
		is_integer(Day) == true, Day > 0, Day =< 31,
		is_integer(Hour) == true, Hour >= 0, Hour =< 23,
		is_integer(Minute) == true, Minute >= 0, Minute =< 59,
		is_integer(Second) == true, Second >= 0, Second =< 59 ->
	Time = a:to_string(a_time:format(rfc822,{date,{{Year,Month,Day},{Hour,Minute,Second}}})),
	[{header,["Last-Modified:",Time]}];
last_modified(current) -> last_modified(erlang:localtime());
last_modified(_) -> a:error(?FUNCTION_NAME(),a000).

%%-----------------------------------
%% @spec expires(Time_in) -> list() | {error,_Reason}.
%% where
%%      Time_in :: integer() | tuple() | current
%% @doc Return a list() within HTTP Header formated for Yaws out() function.
%% Example: "Expires: Fri, 30 Oct 1998 14:19:41 GMT"
-spec expires(Time_in) -> list() | {error,_Reason}
	when Time_in :: pos_integer() | tuple() | current.

expires(Time_in) when is_integer(Time_in) == true, Time_in > 0 ->
	Time = a:to_string(a_time:format(rfc822,{timestamp,a_time:timestamp_to_tuple(Time_in)})),
	[{header,["Expires:",Time]}];
expires({{Year,Month,Day},{Hour,Minute,Second}})
	when
		is_integer(Year) == true, Year > 0,
		is_integer(Month) == true, Month > 0, Month =< 12,
		is_integer(Day) == true, Day > 0, Day =< 31,
		is_integer(Hour) == true, Hour >= 0, Hour =< 23,
		is_integer(Minute) == true, Minute >= 0, Minute =< 59,
		is_integer(Second) == true, Second >= 0, Second =< 59 ->
	Time = a:to_string(a_time:format(rfc822,{date,{{Year,Month,Day},{Hour,Minute,Second}}})),
	[{header,["Expires:",Time]}];
expires(current) -> expires(erlang:localtime());
expires(_) -> a:error(?FUNCTION_NAME(),a000).

%%-----------------------------------
%% @spec cache(Operation) -> list() | {error,_Reason}.
%% where
%%      Operation() = no
%% @doc Return a list() within HTTP headers fromated for Yaws out() function.
-spec cache(Operation) -> list() | {error,_Reason}
	when Operation :: no.

cache(no) ->
	[
		{header,"Cache-Control: no-cache, no-store, must-revalidate"},
		{header,"Pragma: no-cache"},
		expires(1),
		last_modified(current)
	];
cache(_) -> a:error(?FUNCTION_NAME(),a000).

%%-----------------------------------
%% @spec json(Header_type) -> list() | {error,_Reason}
%% where
%%      Header_type() = np_cache | solid
%% @doc Return a list within headers for JSON
-spec json(no_cache) -> list() | {error,_Reason}.

json(no_cache) ->
	[
		cache(no),
		json(solid)
	];
json(solid) ->
	[{header,["Content-Type:","application/json; charset=utf-8"]}];
json(_) -> a:error(?FUNCTION_NAME(),a000).

%%-----------------------------------
%% @spec csv(Type,File_name) -> list() | {error,_Reason}
%% where
%%      Type :: no_cache | solid,
%%      File_name :: unicode:latin1_chardata().
%% @doc Return a list within HTTP headers for CSV file format
-spec csv(Type,File_name) -> list() | {error,_Reason}
	when
		Type :: no_cache | solid,
		File_name :: unicode:latin1_chardata().

csv(Type,File_name) when is_atom(Type) ->
	case io_lib:char_list(File_name) of
		true -> csv_set(Type,File_name);
		false -> a:error(?FUNCTION_NAME(),a000)
	end;
csv(_,_) -> a:error(?FUNCTION_NAME(),a000).
csv_set(no_cache,File_name) ->
	[
		cache(no),
		csv(solid,File_name)
	];
csv_set(solid,File_name) ->
	[
		{header,["Content-Type:","text/csv; charset=utf-8"]},
		{header,["Content-Disposition:",lists:concat(["attachment; filename=",File_name])]}
	];
csv_set(_,_) -> a:error(?FUNCTION_NAME(),a000).

%%-----------------------------------
%% @spec xml(Content_type) -> list() | {error,_Reason}
%% where
%%      Content_typr :: text_xml | application_xml
%% @doc Return list within XML MIME type headers for Yaws out() function
-spec xml(Content_type) -> list() | {error,_Reason}
	when Content_type :: text_xml | application_xml.

xml(text_xml) ->
	[{header,["Content-Type:","text/xml; charset=utf-8"]}];
xml(application_xml) ->
	[{header,["Content-Type:","application/xml; charset=utf-8"]}];
xml(_) -> a:error(?FUNCTION_NAME(),a000).