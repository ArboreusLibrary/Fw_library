%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV (http://alexandr.kirilov.me)
%%% @copyright (C) 2015, Arboreus, (http://arboreus.systems)
%%% @doc
%%%
%%% @end
%%% Created : 21. Jul 2015 21:55
%%%-------------------------------------------------------------------
-module(a_time).
-author("Alexandr KIRILOV (http://alexandr.kirilov.me)").
-vsn("0.0.7.144").

%% Module API
-export([current_date/0,current_year/1,current_month/0,current_day/0,current_dow/1]).
-export([current/0,current/1]).
-export([timestamp/0,timestamp/1,timestamp_to_tuple/1]).
-export([dow/2]).
-export([month/2]).
-export([format/2]).

%% Module Include Start
-include("../Handler/a.hrl").
%% Module Include End


%% ------------------------------------------------
%% Current
%% ------------------------------------------------

%%-----------------------------------
%% @spec current_date() -> tuple()
%% @doc Return a Date() within a tuple() :: {Year,Month,Day}, the part of erlang:localtime()
-spec current_date() -> tuple().

current_date() -> {Date,_}=erlang:localtime(), Date.

%%-----------------------------------
%% @spec current_year(Output_type) -> integer()
%% @doc Return a Year :: integer(), the part of erlang:localtime()
-spec current_year(Output_type) -> integer() | {error,_Error_notice} when
	Output_type :: full | short .

current_year(full) -> {Year,_,_} = current_date(), Year;
current_year(short) -> current_year(full) - trunc(current_year(full)/100)*100;
current_year(_) -> a:error(?FUNCTION_NAME(),a011).

%%-----------------------------------
%% @spec current_month() -> integer()
%% @doc Return a Month() = integer(), the part of erlang:localtime()
-spec current_month() -> integer().

current_month() -> {_,Month,_} = current_date(), Month.

%%-----------------------------------
%% @spec current_day() -> integer()
%% @doc Return a Day :: integer(), the part of erlang:localtime()
-spec current_day() -> integer().

current_day() -> {_,_,Day} = current_date(), Day.

%%-----------------------------------
%% @spec current_dow(View) -> byte()
%% where
%%      View :: full | alpha2 | alpha3.
%% @doc Return the current day of the week within binaries
-spec current_dow(View) -> byte() | {error,_Reason}
	when View :: full | alpha2 | alpha3.

current_dow(View)
	when
		View == full;
		View == alpha2;
		View == alpha3 ->
	dow(calendar:day_of_the_week(current_date()),View);
current_dow(_) -> a:error(?FUNCTION_NAME(),a012).

%%-----------------------------------
%% @spec current() -> tuple()
%% @doc Return a Time() within a tuple() = {Hours,Minutes,Seconds}, the part of erlang:localtime()
-spec current() -> tuple().

current() -> {_,Time}=erlang:localtime(), Time.

%%-----------------------------------
%% @spec current(Format) -> binary()
%% where
%%      Format :: rfc850 | rfc822 | ansi
%% @doc Return a binary within the current time formated by the Format()::atom() from the list
-spec current(Format) -> binary() | {error,_Reason}
	when Format :: rfc850 | rfc822 | ansi.

current(Format)
	when
		Format == rfc850; Format == rfc822; Format == ansi ->
	format(Format,{date,erlang:localtime()});
current(_) -> a:error(?FUNCTION_NAME(),a012).


%% ------------------------------------------------
%% Timestamp
%% ------------------------------------------------

%%-----------------------------------
%% @spec timestamp(Type::binaries) -> byte().
%% @doc Return difned format value of current timestamp
-spec timestamp(Type::binaries) -> byte() | {error,_Reason}.

timestamp(binaries) -> integer_to_binary(timestamp());
timestamp(_) -> a:error(?FUNCTION_NAME(),a012).

%%-----------------------------------
%% @spec timestamp() -> integer()
%% @doc Return current timestamp as integer
-spec timestamp() -> integer().

timestamp() ->
	{Mega,Sec,Micro}=os:timestamp(),
	Mega*1000000000000+Sec*1000000+Micro.

%%-----------------------------------
%% @spec timestamp_to_tuple(Timestamp) -> tuple()
%% @doc Return a tuple within converted Timestamp from integer
-spec timestamp_to_tuple(Timestamp::integer()) -> tuple() | {error,_Reason}.

timestamp_to_tuple(Timestamp) when is_integer(Timestamp) == true, Timestamp > 0 ->
	Mega = Timestamp div 1000000000000,
	Sec = Timestamp div 1000000 rem 1000000,
	Micro = Timestamp rem 1000000,
	{Mega,Sec,Micro};
timestamp_to_tuple(_) -> a:error(?FUNCTION_NAME(),a009).


%% ------------------------------------------------
%% Day of the week
%% ------------------------------------------------

%%-----------------------------------
%% @spec dow(Day_number,View) -> binaries() | {error,_Reason}
%% where
%%      Day_number :: pos_integer(),
%%      View :: full | alpha2 | alpha3.
%% @doc Retuen a binary within day of the week name in defined view
-spec dow(Day_number,View) -> byte() | {error,_Reason}
	when Day_number :: pos_integer(), View :: full | alpha2 | alpha3.

dow(1,full) -> <<"Monday">>;
dow(2,full) -> <<"Tuesday">>;
dow(3,full) -> <<"Wednesday">>;
dow(4,full) -> <<"Thursday">>;
dow(5,full) -> <<"Friday">>;
dow(6,full) -> <<"Saturday">>;
dow(7,full) -> <<"Sunday">>;

dow(1,alpha3) -> <<"Mon">>;
dow(2,alpha3) -> <<"Tue">>;
dow(3,alpha3) -> <<"Wed">>;
dow(4,alpha3) -> <<"Thu">>;
dow(5,alpha3) -> <<"Fri">>;
dow(6,alpha3) -> <<"Sat">>;
dow(7,alpha3) -> <<"Sun">>;

dow(1,alpha2) -> <<"Mo">>;
dow(2,alpha2) -> <<"Tu">>;
dow(3,alpha2) -> <<"Wd">>;
dow(4,alpha2) -> <<"Th">>;
dow(5,alpha2) -> <<"Fr">>;
dow(6,alpha2) -> <<"Sa">>;
dow(7,alpha2) -> <<"Su">>;

dow(Dow,View) ->
	if
		is_integer(Dow) /= true; Dow < 1; Dow > 7 ->
			a:error(?FUNCTION_NAME(),a009);
		is_atom(View) /= true; View /= full; View /= alpha2; View /= alpha3 ->
			a:error(?FUNCTION_NAME(),a012);
		true ->
			a:error(?FUNCTION_NAME(),a000)
	end.


%% ------------------------------------------------
%% Month
%% ------------------------------------------------

%%-----------------------------------
%% @spec month(Month_number,View) -> byte() | {error,_Reason}
%% where
%%      Month_number :: pos_integer(),
%%      View :: full | alpha2 | alpha3.
%% @doc Return binary within month name in defined view
-spec month(Month_number,View) -> byte() | {error,_Reason}
	when Month_number :: pos_integer(), View :: full | alpha2 | alpha3.

month(1,full) -> <<"January">>;
month(2,full) -> <<"February">>;
month(3,full) -> <<"March">>;
month(4,full) -> <<"April">>;
month(5,full) -> <<"May">>;
month(6,full) -> <<"June">>;
month(7,full) -> <<"July">>;
month(8,full) -> <<"August">>;
month(9,full) -> <<"September">>;
month(10,full) -> <<"October">>;
month(11,full) -> <<"November">>;
month(12,full) -> <<"December">>;

month(1,alpha3) -> <<"Jan">>;
month(2,alpha3) -> <<"Feb">>;
month(3,alpha3) -> <<"Mar">>;
month(4,alpha3) -> <<"Apr">>;
month(5,alpha3) -> <<"May">>;
month(6,alpha3) -> <<"Jun">>;
month(7,alpha3) -> <<"Jul">>;
month(8,alpha3) -> <<"Aug">>;
month(9,alpha3) -> <<"Sep">>;
month(10,alpha3) -> <<"Oct">>;
month(11,alpha3) -> <<"Nov">>;
month(12,alpha3) -> <<"Dec">>;

month(1,alpha2) -> <<"Ja">>;
month(2,alpha2) -> <<"Fe">>;
month(3,alpha2) -> <<"Mr">>;
month(4,alpha2) -> <<"Ap">>;
month(5,alpha2) -> <<"Ma">>;
month(6,alpha2) -> <<"Jn">>;
month(7,alpha2) -> <<"Jl">>;
month(8,alpha2) -> <<"Au">>;
month(9,alpha2) -> <<"Se">>;
month(10,alpha2) -> <<"Oc">>;
month(11,alpha2) -> <<"No">>;
month(12,alpha2) -> <<"De">>;

month(Month_number,View) ->
	if
		is_integer(Month_number) /= true, Month_number > 12, Month_number < 1 ->
			a:error(?FUNCTION_NAME(),a009);
		is_atom(View) /= true, View /= full, View /= alpha2, View /= alpha3 ->
			a:error(?FUNCTION_NAME(),a012);
		true ->
			a:error(?FUNCTION_NAME(),a000)
	end.


%% ------------------------------------------------
%% Format
%% ------------------------------------------------

%%-----------------------------------
%% @spec format(View,Time_in) -> byte() | {error,_Reason}
%% where
%%      View = ansi | rfc850 | rfc822
%%      Time = {timestamp | date,{Time_data()}}
%%      Time_data() = tuple()
%% @doc Return a binary within a formated time
-spec format(View,Time_in) -> byte() | {error,_Reason}
	when
		View :: full | alpha2 | alpha3,
		Time_in :: {timestamp,{Mega,Seconds,Micro}} | {date,Time},
		Mega :: integer(), Seconds :: integer(), Micro :: integer(),
		Time :: tuple().

format(ansi,Time_in) ->
	case Time_in of
		{timestamp,{Mega,Seconds,Micro}} ->
			{{Year,Month,Day},{Hour,Minute,Second}} = calendar:now_to_local_time({Mega,Seconds,Micro});
		{date,Time} ->
			{{Year,Month,Day},{Hour,Minute,Second}} = Time
	end,
	Dow = dow(calendar:day_of_the_week(Year,Month,Day),alpha3),
	Out_month = month(Month,alpha3),
	Out_day = integer_to_binary(Day),
	Out_hours = format(hour,Hour),
	Out_minutes = format(min,Minute),
	Out_seconds = format(sec,Second),
	Out_year = integer_to_binary(Year),
	<<Dow/binary," ",Out_month/binary," ",Out_day/binary," ",
	Out_hours/binary,":",Out_minutes/binary,":",Out_seconds/binary," ",
	Out_year/binary>>;

format(rfc850,Time_in) ->
	case Time_in of
		{timestamp,{Mega,Seconds,Micro}} ->
			{{Year,Month,Day},{Hour,Minute,Second}} = calendar:now_to_local_time({Mega,Seconds,Micro});
		{date,Time} ->
			{{Year,Month,Day},{Hour,Minute,Second}} = Time
	end,
	Dow = dow(calendar:day_of_the_week(Year,Month,Day),full),
	Out_month = month(Month,alpha3),
	Out_day = format(day,Day),
	Out_hours = format(hour,Hour),
	Out_minutes = format(min,Minute),
	Out_seconds = format(sec,Second),
	Out_year = format(year_short,Year),
	<<Dow/binary,", ",Out_day/binary,"-",Out_month/binary,"-",Out_year/binary," ",
	Out_hours/binary,":",Out_minutes/binary,":",Out_seconds/binary," GMT">>;

format(rfc822,Time_in) ->
	case Time_in of
		{timestamp,{Mega,Seconds,Micro}} ->
			{{Year,Month,Day},{Hour,Minute,Second}} = calendar:now_to_local_time({Mega,Seconds,Micro});
		{date,Time} ->
			{{Year,Month,Day},{Hour,Minute,Second}} = Time
	end,
	Dow = dow(calendar:day_of_the_week(Year,Month,Day),alpha3),
	Out_month = month(Month,alpha3),
	Out_day = format(day,Day),
	Out_hours = format(hour,Hour),
	Out_minutes = format(min,Minute),
	Out_seconds = format(sec,Second),
	Out_year = integer_to_binary(Year),
	<<Dow/binary,", ",Out_day/binary," ",Out_month/binary," ",Out_year/binary," ",
	Out_hours/binary,":",Out_minutes/binary,":",Out_seconds/binary," GMT">>;

format(Measure,0)
	when Measure == hour; Measure == min; Measure == sec -> <<"00">>;
format(Measure,1)
	when Measure == hour; Measure == min; Measure == sec; Measure == month; Measure == day -> <<"01">>;
format(Measure,2)
	when Measure == hour; Measure == min; Measure == sec; Measure == month; Measure == day -> <<"02">>;
format(Measure,3)
	when Measure == hour; Measure == min; Measure == sec; Measure == month; Measure == day -> <<"03">>;
format(Measure,4)
	when Measure == hour; Measure == min; Measure == sec; Measure == month; Measure == day -> <<"04">>;
format(Measure,5)
	when Measure == hour; Measure == min; Measure == sec; Measure == month; Measure == day -> <<"05">>;
format(Measure,6)
	when Measure == hour; Measure == min; Measure == sec; Measure == month; Measure == day -> <<"06">>;
format(Measure,7)
	when Measure == hour; Measure == min; Measure == sec; Measure == month; Measure == day -> <<"07">>;
format(Measure,8)
	when Measure == hour; Measure == min; Measure == sec; Measure == month; Measure == day -> <<"08">>;
format(Measure,9)
	when Measure == hour; Measure == min; Measure == sec; Measure == month; Measure == day -> <<"09">>;

format(year,Year) -> a:bin(Year);
format(year_short,Year) ->
	binary:part(integer_to_binary(Year),2,2);

format(day,Day) when is_integer(Day) == true, Day >9, Day =< 31 ->
	Out_day = integer_to_binary(Day),
	<<Out_day/binary>>;

format(month,Month) when is_integer(Month) == true, Month > 9, Month =< 12 ->
	Out_hours = integer_to_binary(Month),
	<<Out_hours/binary>>;
format(month,_) -> a:error(?FUNCTION_NAME(),a009);

format(hour,Hours) when is_integer(Hours) == true, Hours > 9, Hours =< 23 ->
	Out_hours = integer_to_binary(Hours),
	<<Out_hours/binary>>;
format(hour,_) -> a:error(?FUNCTION_NAME(),a009);

format(min,Minutes) when is_integer(Minutes) == true, Minutes > 9, Minutes =< 59 ->
	Out_minutes = integer_to_binary(Minutes),
	<<Out_minutes/binary>>;
format(min,_) -> a:error(?FUNCTION_NAME(),a009);

format(sec,Seconds) when is_integer(Seconds) == true, Seconds > 9, Seconds =< 59 ->
	Out_seconds = integer_to_binary(Seconds),
	<<Out_seconds/binary>>;
format(sec,_) -> a:error(?FUNCTION_NAME(),a009);

format(_,_) -> a:error(?FUNCTION_NAME(),a000).