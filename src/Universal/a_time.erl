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
-vsn("0.0.19.284").

%% Module API
-export([
	current_date/0,current_year/1,current_month/0,current_day/0,current_dow/1,
	current/0,current/1,
	timestamp/0,timestamp/1,timestamp_to_tuple/1,from_timestamp/2,
	second/1,
	minute/1,
	hour/1,
	day/1,
	dow/1,dow/2,
	month/1,month/2,
	year/2,
	format/2,
	from_formated/3
]).

%% Module Include Start
-include("../Handler/a.hrl").
%% Module Include End


%% ------------------------------------------------
%% Current
%% ------------------------------------------------

%%-----------------------------------
%% @doc Return a Date() within a tuple() :: {Year,Month,Day}, the part of erlang:localtime()
-spec current_date() -> tuple().

current_date() -> {Date,_}=erlang:localtime(), Date.


%%-----------------------------------
%% @doc Return a Year :: integer(), the part of erlang:localtime()
-spec current_year(Output_type) -> integer() | {error,_Error_notice} when
	Output_type :: full | short .

current_year(full) -> {Year,_,_} = current_date(), Year;
current_year(short) -> current_year(full) - trunc(current_year(full)/100)*100;
current_year(_) -> a:error(?FUNCTION_NAME(),a011).


%%-----------------------------------
%% @doc Return a Month() = integer(), the part of erlang:localtime()
-spec current_month() -> integer().

current_month() -> {_,Month,_} = current_date(), Month.


%%-----------------------------------
%% @doc Return a Day :: integer(), the part of erlang:localtime()
-spec current_day() -> integer().

current_day() -> {_,_,Day} = current_date(), Day.


%%-----------------------------------
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
%% @doc Return a Time() within a tuple() = {Hours,Minutes,Seconds}, the part of erlang:localtime()
-spec current() -> tuple().

current() -> {_,Time}=erlang:localtime(), Time.


%%-----------------------------------
%% @doc Return a binary within the current time formated by the Format()::atom() from the list
-spec current(Format) -> binary() | {error,_Reason}
	when
		Format :: atom() | tuple().

current(timestamp) -> timestamp();
current(Format) ->
	case format(Format,{date_tuple,erlang:localtime()}) of
		{error,_} -> a:error(?FUNCTION_NAME(),a012);
		Current -> Current
	end.


%% ------------------------------------------------
%% Timestamp
%% ------------------------------------------------

%%-----------------------------------
%% @doc Return difned format value of current timestamp
-spec timestamp(Type::binaries) -> byte() | {error,_Reason}.

timestamp(binaries) -> integer_to_binary(timestamp());
timestamp(_) -> a:error(?FUNCTION_NAME(),a012).


%%-----------------------------------
%% @doc Return current timestamp as integer
-spec timestamp() -> integer().

timestamp() ->
	{Mega,Sec,Micro}=os:timestamp(),
	Mega*1000000000000+Sec*1000000+Micro.


%%-----------------------------------
%% @doc Return a tuple within converted Timestamp from integer
-spec timestamp_to_tuple(Timestamp::integer()) -> tuple() | {error,_Reason}.

timestamp_to_tuple(Timestamp) when is_integer(Timestamp) == true, Timestamp > 0 ->
	Mega = Timestamp div 1000000000000,
	Sec = Timestamp div 1000000 rem 1000000,
	Micro = Timestamp rem 1000000,
	{Mega,Sec,Micro};
timestamp_to_tuple(_) -> a:error(?FUNCTION_NAME(),a009).


%%-----------------------------------
%% @doc return integer within timestamp from fromated date tuple
-spec to_timestamp(Data) -> integer() | {error,_Reason}
	when
		Data :: {{Year,Month,Day},{Hours,Minutes,Seconds}},
		Year :: integer(), Month :: integer(), Day :: integer(),
		Hours :: integer(), Minutes :: integer(), Seconds :: integer().

to_timestamp({{Year,Month,Day},{Hours,Minutes,Seconds}}) ->
	(calendar:datetime_to_gregorian_seconds(
		{{Year,Month,Day},{Hours,Minutes,Seconds}}
	) - 62167219200) * 1000000;
to_timestamp(_) -> a:error(?FUNCTION_NAME(),a000).


%%-----------------------------------
%% @doc Return formated time from timestamp
-spec from_timestamp(Time_format,Timestamp) -> unicode:latin1_binary()| {error,_Reason}
	when
		Time_format :: atom(),
		Timestamp :: integer().

from_timestamp(date_tuple,Timestamp) when is_integer(Timestamp), Timestamp >= 1 ->
	calendar:gregorian_seconds_to_datetime(Timestamp div 1000000 + 62167219200);
from_timestamp(Time_format,Timestamp) when is_integer(Timestamp), Timestamp >= 1 ->
	case format(Time_format,{date,from_timestamp(date_tuple,Timestamp)}) of
		{error,_} -> a:error(?FUNCTION_NAME(),a000);
		Formated_time -> Formated_time
	end;
from_timestamp(_,_) -> a:error(?FUNCTION_NAME(),a000).

%% ------------------------------------------------
%% Time
%% ------------------------------------------------

%%-----------------------------------
%% @doc
-spec second(Second) -> integer() | {error,_Reason}
	when
		Second :: string() | unicode:latin1_binary().

second(Second) when Second == <<"0">>, Second == <<"00">>, Second == "0", Second == "00" -> 0;
second(Second) when Second == <<"1">>, Second == <<"01">>, Second == "1", Second == "01" -> 1;
second(Second) when Second == <<"2">>, Second == <<"02">>, Second == "2", Second == "02" -> 2;
second(Second) when Second == <<"3">>, Second == <<"03">>, Second == "3", Second == "03" -> 3;
second(Second) when Second == <<"4">>, Second == <<"04">>, Second == "4", Second == "04" -> 4;
second(Second) when Second == <<"5">>, Second == <<"05">>, Second == "5", Second == "05" -> 5;
second(Second) when Second == <<"6">>, Second == <<"06">>, Second == "6", Second == "06" -> 6;
second(Second) when Second == <<"7">>, Second == <<"07">>, Second == "7", Second == "07" -> 7;
second(Second) when Second == <<"8">>, Second == <<"08">>, Second == "8", Second == "08" -> 8;
second(Second) when Second == <<"9">>, Second == <<"09">>, Second == "9", Second == "09" -> 9;
second(Second) when is_binary(Second) ->
	try
		Second_integer = binary_to_integer(Second),
		if
			Second_integer >= 0, Second_integer =< 59 -> Second_integer;
			true -> a:error(?FUNCTION_NAME(),a000)
		end
	catch _:_ -> a:error(?FUNCTION_NAME(),a000) end;
second(Second) when is_list(Second) ->
	case io_lib:char_list(Second) of
		true ->
			Second_integer = list_to_integer(Second),
			if
				Second_integer >= 0, Second_integer =< 59 -> Second_integer;
				true -> a:error(?FUNCTION_NAME(),a000)
			end
	end;
second(_) -> a:error(?FUNCTION_NAME(),a000).


%%-----------------------------------
%% @doc Return integer from formated minute
-spec minute(Minute) -> integer() | {error,_Reason}
	when
		Minute :: string() | unicode:latin1_binary().

minute(Minute) -> second(Minute).


%%-----------------------------------
%% @doc Return integer from formated hour
-spec hour(Hour) -> integer() | {error,_Reason}
	when
		Hour :: string() | unicode:latin1_binary().

hour(Hour) when Hour == <<"0">>, Hour == <<"00">>, Hour == "0", Hour == "00" -> 0;
hour(Hour) when Hour == <<"1">>, Hour == <<"01">>, Hour == "1", Hour == "01" -> 1;
hour(Hour) when Hour == <<"2">>, Hour == <<"02">>, Hour == "2", Hour == "02" -> 2;
hour(Hour) when Hour == <<"3">>, Hour == <<"03">>, Hour == "3", Hour == "03" -> 3;
hour(Hour) when Hour == <<"4">>, Hour == <<"04">>, Hour == "4", Hour == "04" -> 4;
hour(Hour) when Hour == <<"5">>, Hour == <<"05">>, Hour == "5", Hour == "05" -> 5;
hour(Hour) when Hour == <<"6">>, Hour == <<"06">>, Hour == "6", Hour == "06" -> 6;
hour(Hour) when Hour == <<"7">>, Hour == <<"07">>, Hour == "7", Hour == "07" -> 7;
hour(Hour) when Hour == <<"8">>, Hour == <<"08">>, Hour == "8", Hour == "08" -> 8;
hour(Hour) when Hour == <<"9">>, Hour == <<"09">>, Hour == "9", Hour == "09" -> 9;
hour(Hour) when is_binary(Hour) ->
	try
		Hour_integer = binary_to_integer(Hour),
		if
			Hour_integer >= 0, Hour_integer =< 23 -> Hour_integer;
			true -> a:error(?FUNCTION_NAME(),a000)
		end
	catch _:_ -> a:error(?FUNCTION_NAME(),a000) end;
hour(Hour) when is_list(Hour) ->
	case io_lib:char_list(Hour) of
		true ->
			Hour_integer = list_to_integer(Hour),
			if
				Hour_integer >= 1, Hour_integer =< 31 -> Hour_integer;
				true -> a:error(?FUNCTION_NAME(),a000)
			end
	end;
hour(_) -> a:error(?FUNCTION_NAME(),a000).


%%-----------------------------------
%% @doc Return integer within converted day
-spec day(Day) -> integer() | {error,_Reason}
	when
		Day :: string() | unicode:latin1_binary().

day(Day) when Day == <<"1">>, Day == <<"01">>, Day == "1", Day == "01" -> 1;
day(Day) when Day == <<"2">>, Day == <<"02">>, Day == "2", Day == "02" -> 2;
day(Day) when Day == <<"3">>, Day == <<"03">>, Day == "3", Day == "03" -> 3;
day(Day) when Day == <<"4">>, Day == <<"04">>, Day == "4", Day == "04" -> 4;
day(Day) when Day == <<"5">>, Day == <<"05">>, Day == "5", Day == "05" -> 5;
day(Day) when Day == <<"6">>, Day == <<"06">>, Day == "6", Day == "06" -> 6;
day(Day) when Day == <<"7">>, Day == <<"07">>, Day == "7", Day == "07" -> 7;
day(Day) when Day == <<"8">>, Day == <<"08">>, Day == "8", Day == "08" -> 8;
day(Day) when Day == <<"9">>, Day == <<"09">>, Day == "9", Day == "09" -> 9;
day(Day) when is_binary(Day) ->
	try
	    Day_integer = binary_to_integer(Day),
		if
			Day_integer >= 1, Day_integer =< 31 -> Day_integer;
			true -> a:error(?FUNCTION_NAME(),a000)
		end
	catch _:_ -> a:error(?FUNCTION_NAME(),a000) end;
day(Day) when is_list(Day) ->
	case io_lib:char_list(Day) of
		true ->
			Day_integer = list_to_integer(Day),
			if
				Day_integer >= 1, Day_integer =< 31 -> Day_integer;
				true -> a:error(?FUNCTION_NAME(),a000)
			end
	end;
day(_) -> a:error(?FUNCTION_NAME(),a000).


%% ------------------------------------------------
%% Day of the week
%% ------------------------------------------------

%%-----------------------------------
%% @doc Return integer within number of day the week
-spec dow(Dow::unicode:latin1_binary()) -> integer() | {error,_Reason}.

dow(Dow)
	when
		Dow == <<"1">>, Dow == <<"01">>, Dow == "1", Dow == "01",
		Dow == <<"Monday">>; Dow == <<"Mon">>; Dow == <<"Mo">>,
		Dow == "Monday"; Dow == "Mon"; Dow == "Mo" -> 1;
dow(Dow)
	when
		Dow == <<"2">>, Dow == <<"02">>, Dow == "2", Dow == "02",
		Dow == <<"Tuesday">>; Dow == <<"Tue">>; Dow == <<"Tu">>,
		Dow == "Tuesday"; Dow == "Tue"; Dow == "Tu" -> 2;
dow(Dow)
	when
		Dow == <<"3">>, Dow == <<"03">>, Dow == "3", Dow == "03",
		Dow == <<"Wednesday">>; Dow == <<"Wed">>; Dow == <<"Wd">>,
		Dow == "Wednesday"; Dow == "Wed"; Dow == "Wd" -> 3;
dow(Dow)
	when
		Dow == <<"4">>, Dow == <<"04">>, Dow == "4", Dow == "04",
		Dow == <<"Thursday">>; Dow == <<"Thu">>; Dow == <<"Th">>,
		Dow == "Thursday"; Dow == "Thu"; Dow == "Th" -> 4;
dow(Dow)
	when
		Dow == <<"5">>, Dow == <<"05">>, Dow == "5", Dow == "05",
		Dow == <<"Friday">>; Dow == <<"Fri">>; Dow == <<"Fr">>,
		Dow == "Friday"; Dow == "Fri"; Dow == "Fr" -> 5;
dow(Dow)
	when
		Dow == <<"6">>, Dow == <<"06">>, Dow == "6", Dow == "06",
		Dow == <<"Saturday">>; Dow == <<"Sat">>; Dow == <<"Sa">>,
		Dow == "Saturday"; Dow == "Sat"; Dow == "Sa" -> 6;
dow(Dow)
	when
		Dow == <<"7">>, Dow == <<"07">>, Dow == "7", Dow == "07",
		Dow == <<"Sunday">>; Dow == <<"Sun">>; Dow == <<"Su">>,
		Dow == "Sunday"; Dow == "Sun"; Dow == "Su" -> 7;
dow(_) -> a:error(?FUNCTION_NAME(),a000).


%%-----------------------------------
%% @doc Return a binary within day of the week name in defined view
-spec dow(Day_number,View) -> byte() | {error,_Reason}
	when
		Day_number :: pos_integer(),
		View :: full | alpha2 | alpha3.

dow(1,full) -> <<"Monday">>; dow(2,full) -> <<"Tuesday">>; dow(3,full) -> <<"Wednesday">>;
dow(4,full) -> <<"Thursday">>; dow(5,full) -> <<"Friday">>; dow(6,full) -> <<"Saturday">>;
dow(7,full) -> <<"Sunday">>;

dow(1,alpha3) -> <<"Mon">>; dow(2,alpha3) -> <<"Tue">>; dow(3,alpha3) -> <<"Wed">>;
dow(4,alpha3) -> <<"Thu">>; dow(5,alpha3) -> <<"Fri">>; dow(6,alpha3) -> <<"Sat">>;
dow(7,alpha3) -> <<"Sun">>;

dow(1,alpha2) -> <<"Mo">>; dow(2,alpha2) -> <<"Tu">>; dow(3,alpha2) -> <<"Wd">>;
dow(4,alpha2) -> <<"Th">>; dow(5,alpha2) -> <<"Fr">>; dow(6,alpha2) -> <<"Sa">>;
dow(7,alpha2) -> <<"Su">>;

dow(_,_) -> a:error(?FUNCTION_NAME(),a000).


%% ------------------------------------------------
%% Month
%% ------------------------------------------------


%%-----------------------------------
%% @doc Return integer within month number from unicode binary
-spec month(Month::unicode:latin1_binary()) -> integer() | {error,_Reason}.

month(Month)
	when
		Month == <<"1">>, Month == <<"01">>, Month == "1", Month == "01",
		Month == <<"January">>; Month == <<"Jan">>; Month == <<"Ja">>,
		Month == "January"; Month == "Jan"; Month == "Ja" -> 1;
month(Month)
	when
		Month == <<"2">>, Month == <<"02">>, Month == "2", Month == "02",
		Month == <<"February">>; Month == <<"Feb">>; Month == <<"Fe">>,
		Month == "February"; Month == "Feb"; Month == "Fe" -> 2;
month(Month)
	when
		Month == <<"3">>, Month == <<"03">>, Month == "3", Month == "03",
		Month == <<"March">>; Month == <<"Mar">>; Month == <<"Mr">>,
		Month == "March"; Month == "Mar"; Month == "Mr" -> 3;
month(Month)
	when
		Month == <<"4">>, Month == <<"04">>, Month == "4", Month == "04",
		Month == <<"April">>; Month == <<"Apr">>; Month == <<"Ap">>,
		Month == "April"; Month == "Apr"; Month == "Ap" -> 4;
month(Month)
	when
		Month == <<"5">>, Month == <<"05">>, Month == "5", Month == "05",
		Month == <<"May">>; Month == <<"May">>; Month == <<"Ma">>,
		Month == "May"; Month == "May"; Month == "Ma" -> 5;
month(Month)
	when
		Month == <<"6">>, Month == <<"06">>, Month == "6", Month == "06",
		Month == <<"June">>; Month == <<"Jun">>; Month == <<"Jn">>,
		Month == "June"; Month == "Jun"; Month == "Jn" -> 6;
month(Month)
	when
		Month == <<"7">>, Month == <<"07">>, Month == "7", Month == "07",
		Month == <<"July">>; Month == <<"Jul">>; Month == <<"Jl">>,
		Month == "July"; Month == "Jul"; Month == "Jl" -> 7;
month(Month)
	when
		Month == <<"8">>, Month == <<"08">>, Month == "8", Month == "08",
		Month == <<"August">>; Month == <<"Aug">>; Month == <<"Au">>,
		Month == "August"; Month == "Aug"; Month == "Au" -> 8;
month(Month)
	when
		Month == <<"9">>, Month == <<"09">>, Month == "9", Month == "09",
		Month == <<"September">>; Month == <<"Sep">>; Month == <<"Se">>,
		Month == "September"; Month == "Sep"; Month == "Se" -> 9;
month(Month)
	when
		Month == <<"10">>, Month == "10",
		Month == <<"October">>; Month == <<"Oct">>; Month == <<"Oc">>,
		Month == "October"; Month == "Oct"; Month == "Oc" -> 10;
month(Month)
	when
		Month == <<"11">>, Month == "11",
		Month == <<"November">>; Month == <<"Nov">>; Month == <<"No">>,
		Month == "November"; Month == "Nov"; Month == "No" -> 11;
month(Month)
	when
		Month == <<"12">>, Month == "12",
		Month == <<"December">>; Month == <<"Dec">>; Month == <<"De">>,
		Month == "December"; Month == "Dec"; Month == "De" -> 12;
month(_) -> a:error(?FUNCTION_NAME(),a000).


%%-----------------------------------
%% @doc Return binary within month name in defined view
-spec month(Month_number,View) -> byte() | {error,_Reason}
	when
		Month_number :: pos_integer(),
		View :: full | alpha2 | alpha3.

month(1,full) -> <<"January">>; month(2,full) -> <<"February">>; month(3,full) -> <<"March">>;
month(4,full) -> <<"April">>; month(5,full) -> <<"May">>; month(6,full) -> <<"June">>;
month(7,full) -> <<"July">>; month(8,full) -> <<"August">>; month(9,full) -> <<"September">>;
month(10,full) -> <<"October">>; month(11,full) -> <<"November">>; month(12,full) -> <<"December">>;

month(1,alpha3) -> <<"Jan">>; month(2,alpha3) -> <<"Feb">>; month(3,alpha3) -> <<"Mar">>;
month(4,alpha3) -> <<"Apr">>; month(5,alpha3) -> <<"May">>; month(6,alpha3) -> <<"Jun">>;
month(7,alpha3) -> <<"Jul">>; month(8,alpha3) -> <<"Aug">>; month(9,alpha3) -> <<"Sep">>;
month(10,alpha3) -> <<"Oct">>; month(11,alpha3) -> <<"Nov">>; month(12,alpha3) -> <<"Dec">>;

month(1,alpha2) -> <<"Ja">>; month(2,alpha2) -> <<"Fe">>; month(3,alpha2) -> <<"Mr">>;
month(4,alpha2) -> <<"Ap">>; month(5,alpha2) -> <<"Ma">>; month(6,alpha2) -> <<"Jn">>;
month(7,alpha2) -> <<"Jl">>; month(8,alpha2) -> <<"Au">>; month(9,alpha2) -> <<"Se">>;
month(10,alpha2) -> <<"Oc">>; month(11,alpha2) -> <<"No">>; month(12,alpha2) -> <<"De">>;

month(_,_) -> a:error(?FUNCTION_NAME(),a000).

%% ------------------------------------------------
%% Year
%% ------------------------------------------------

%%-----------------------------------
%% @doc Return integer from unicode binary chars
-spec year(Type,Year) -> integer() | {error,_Reason}
	when
		Type :: to_string | to_binary | to_integer,
		Year :: any().

year(to_string,Year) ->
	try
		case a:to_string(Year) of
			{error,Reason} -> {error,Reason};
			Year_binary -> Year_binary
		end
	catch _:_ -> a:error(?FUNCTION_NAME(),a000) end;
year(to_binary,Year) ->
	try
		case a:to_binary(Year) of
			{error,Reason} -> {error,Reason};
			Year_binary -> Year_binary
		end
	catch _:_ -> a:error(?FUNCTION_NAME(),a000) end;
year(to_integer,Year) ->
	try
		Year_integer = a:to_integer(Year),
		case Year_integer of
			{error,Reason} -> {error,Reason};
			_ ->
				if
					Year_integer > 0 -> Year_integer;
					true -> a:error(?FUNCTION_NAME(),a000)
				end
		end
	catch _:_ -> a:error(?FUNCTION_NAME(),a010) end;
year(_,_) -> a:error(?FUNCTION_NAME(),a000).

%% ------------------------------------------------
%% Format
%% ------------------------------------------------

%%-----------------------------------
%% @doc Return a binary within a formated time
-spec format(View,Time_in) -> byte() | {error,_Reason}
	when
		View :: atom() | tuple(),
		Time_in :: tuple() | integer().

format(View,{timestamp,Timestamp})
	when is_integer(Timestamp),Timestamp >= 1 ->
	format(View,{date_tuple,from_timestamp(date_tuple,Timestamp)});

format(View,{timestamp_tuple,{Mega,Seconds,Micro}})
	when is_integer(Mega),is_integer(Seconds),is_integer(Micro) ->
	format(View,{date_tuple,calendar:now_to_local_time({Mega,Seconds,Micro})});

format(ansi,{date_tuple,{{Year,Month,Day},{Hour,Minute,Second}}}) ->
	<<(dow(calendar:day_of_the_week(Year,Month,Day),alpha3))/binary," ",
		(month(Month,alpha3))/binary," ",(format_element(day,Day))/binary," ",
		(format_element(hour,Hour))/binary,":",(format_element(min,Minute))/binary,":",
		(format_element(sec,Second))/binary," ",(format_element(year,Year))/binary>>;

format(rfc850,{date_tuple,{{Year,Month,Day},{Hour,Minute,Second}}}) ->
	<<(dow(calendar:day_of_the_week(Year,Month,Day),full))/binary,", ",
		(format_element(day,Day))/binary,"-",(month(Month,alpha3))/binary,"-",
		(format_element(year_short,Year))/binary," ",(format_element(hour,Hour))/binary,":",
		(format_element(min,Minute))/binary,":",(format_element(sec,Second))/binary," GMT">>;

format(rfc822,{date_tuple,{{Year,Month,Day},{Hour,Minute,Second}}}) ->
	<<(dow(calendar:day_of_the_week(Year,Month,Day),alpha3))/binary,", ",
		(format_element(day,Day))/binary," ",(month(Month,alpha3))/binary," ",
		(format_element(year,Year))/binary," ",(format_element(hour,Hour))/binary,":",
		(format_element(min,Minute))/binary,":",(format_element(sec,Second))/binary," GMT">>;

format({iso8601,"YYYY-MM"},{date_tuple,{{Year,Month,_},{_,_,_}}}) ->
	<<(format_element(year,Year))/binary,("-")/utf8,(format_element(month,Month))/binary>>;
format({iso8601,"YYYY-MM-DD"},{date_tuple,{{Year,Month,Day},{_,_,_}}}) ->
	<<(format_element(year,Year))/binary,("-")/utf8,
		(format_element(month,Month))/binary,("-")/utf8,
		(format_element(day,Day))/binary>>;
format({iso8601,"YY-MM-DD"},{date_tuple,{{Year,Month,Day},{_,_,_}}}) ->
	<<(format_element(year_short,Year))/binary,("-")/utf8,
		(format_element(month,Month))/binary,("-")/utf8,
		(format_element(day,Day))/binary>>;

format({iso8601,"YYYY/MM"},{date_tuple,{{Year,Month,_},{_,_,_}}}) ->
	<<(format_element(year,Year))/binary,("/")/utf8,(format_element(month,Month))/binary>>;
format({iso8601,"YYYY/MM/DD"},{date_tuple,{{Year,Month,Day},{_,_,_}}}) ->
	<<(format_element(year,Year))/binary,("/")/utf8,
		(format_element(month,Month))/binary,("/")/utf8,
		(format_element(day,Day))/binary>>;
format({iso8601,"YY/MM/DD"},{date_tuple,{{Year,Month,Day},{_,_,_}}}) ->
	<<(format_element(year_short,Year))/binary,("/")/utf8,
		(format_element(month,Month))/binary,("/")/utf8,
		(format_element(day,Day))/binary>>;

format({iso8601,"YYYY.MM"},{date_tuple,{{Year,Month,_},{_,_,_}}}) ->
	<<(format_element(year,Year))/binary,(".")/utf8,(format_element(month,Month))/binary>>;
format({iso8601,"YYYY.MM.DD"},{date_tuple,{{Year,Month,Day},{_,_,_}}}) ->
	<<(format_element(year,Year))/binary,(".")/utf8,
		(format_element(month,Month))/binary,(".")/utf8,
		(format_element(day,Day))/binary>>;
format({iso8601,"YY.MM.DD"},{date_tuple,{{Year,Month,Day},{_,_,_}}}) ->
	<<(format_element(year_short,Year))/binary,(".")/utf8,
		(format_element(month,Month))/binary,(".")/utf8,
		(format_element(day,Day))/binary>>;

format({iso8601,"YYYYMM"},{date_tuple,{{Year,Month,_},{_,_,_}}}) ->
	<<(format_element(year,Year))/binary,(format_element(month,Month))/binary>>;
format({iso8601,"YYYYMMDD"},{date_tuple,{{Year,Month,Day},{_,_,_}}}) ->
	<<(format_element(year,Year))/binary,
		(format_element(month,Month))/binary,
		(format_element(day,Day))/binary>>;
format({iso8601,"YYMMDD"},{date_tuple,{{Year,Month,Day},{_,_,_}}}) ->
	<<(format_element(year_short,Year))/binary,
		(format_element(month,Month))/binary,
		(format_element(day,Day))/binary>>;

format({iso8601,"MM-YYYY"},{date_tuple,{{Year,Month,_},{_,_,_}}}) ->
	<<(format_element(month,Month))/binary,("-")/utf8,(format_element(year,Year))/binary>>;
format({iso8601,"DD-MM-YYYY"},{date_tuple,{{Year,Month,Day},{_,_,_}}}) ->
	<<(format_element(day,Day))/binary,("-")/utf8,
		(format_element(month,Month))/binary,("-")/utf8,
		(format_element(year,Year))/binary>>;
format({iso8601,"DD-MM-YY"},{date_tuple,{{Year,Month,Day},{_,_,_}}}) ->
	<<(format_element(day,Day))/binary,("-")/utf8,
		(format_element(month,Month))/binary,("-")/utf8,
		(format_element(year_short,Year))/binary>>;

format({iso8601,"MM/YYYY"},{date_tuple,{{Year,Month,_},{_,_,_}}}) ->
	<<(format_element(month,Month))/binary,("/")/utf8,(format_element(year,Year))/binary>>;
format({iso8601,"DD/MM/YYYY"},{date_tuple,{{Year,Month,Day},{_,_,_}}}) ->
	<<(format_element(day,Day))/binary,("/")/utf8,
		(format_element(month,Month))/binary,("/")/utf8,
		(format_element(year,Year))/binary>>;
format({iso8601,"DD/MM/YY"},{date_tuple,{{Year,Month,Day},{_,_,_}}}) ->
	<<(format_element(day,Day))/binary,("/")/utf8,
		(format_element(month,Month))/binary,("/")/utf8,
		(format_element(year_short,Year))/binary>>;

format({iso8601,"MM.YYYY"},{date_tuple,{{Year,Month,_},{_,_,_}}}) ->
	<<(format_element(month,Month))/binary,(".")/utf8,(format_element(year,Year))/binary>>;
format({iso8601,"DD.MM.YYYY"},{date_tuple,{{Year,Month,Day},{_,_,_}}}) ->
	<<(format_element(day,Day))/binary,(".")/utf8,
		(format_element(month,Month))/binary,(".")/utf8,
		(format_element(year,Year))/binary>>;
format({iso8601,"DD.MM.YY"},{date_tuple,{{Year,Month,Day},{_,_,_}}}) ->
	<<(format_element(day,Day))/binary,(".")/utf8,
		(format_element(month,Month))/binary,(".")/utf8,
		(format_element(year_short,Year))/binary>>;

format({iso8601,"MMYYYY"},{date_tuple,{{Year,Month,_},{_,_,_}}}) ->
	<<(format_element(month,Month))/binary,(".")/utf8,(format_element(year,Year))/binary>>;
format({iso8601,"DDMMYYYY"},{date_tuple,{{Year,Month,Day},{_,_,_}}}) ->
	<<(format_element(day,Day))/binary,(".")/utf8,
		(format_element(month,Month))/binary,(".")/utf8,
		(format_element(year,Year))/binary>>;
format({iso8601,"DDMMYY"},{date_tuple,{{Year,Month,Day},{_,_,_}}}) ->
	<<(format_element(day,Day))/binary,(".")/utf8,
		(format_element(month,Month))/binary,(".")/utf8,
		(format_element(year_short,Year))/binary>>;

format({iso8601,"HH:MM"},{date_tuple,{{_,_,_},{Hour,Minute,_}}}) ->
	<<(format_element(hour,Hour))/binary,(":")/utf8,(format_element(min,Minute))/binary>>;
format({iso8601,"HH.MM"},{date_tuple,{{_,_,_},{Hour,Minute,_}}}) ->
	<<(format_element(hour,Hour))/binary,(".")/utf8,(format_element(min,Minute))/binary>>;
format({iso8601,"HH,MM"},{date_tuple,{{_,_,_},{Hour,Minute,_}}}) ->
	<<(format_element(hour,Hour))/binary,(",")/utf8,(format_element(min,Minute))/binary>>;
format({iso8601,"HHMM"},{date_tuple,{{_,_,_},{Hour,Minute,_}}}) ->
	<<(format_element(hour,Hour))/binary,(format_element(min,Minute))/binary>>;

format({iso8601,"HH:MM:SS"},{date_tuple,{{_,_,_},{Hour,Minute,Second}}}) ->
	<<(format_element(hour,Hour))/binary,(":")/utf8,
		(format_element(min,Minute))/binary,(":")/utf8,
		(format_element(sec,Second))/binary>>;
format({iso8601,"HH.MM.SS"},{date_tuple,{{_,_,_},{Hour,Minute,Second}}}) ->
	<<(format_element(hour,Hour))/binary,(".")/utf8,
		(format_element(min,Minute))/binary,(".")/utf8,
		(format_element(sec,Second))/binary>>;
format({iso8601,"HH,MM,SS"},{date_tuple,{{_,_,_},{Hour,Minute,Second}}}) ->
	<<(format_element(hour,Hour))/binary,(",")/utf8,
		(format_element(min,Minute))/binary,(",")/utf8,
		(format_element(sec,Second))/binary>>;
format({iso8601,"HHMMSS"},{date_tuple,{{_,_,_},{Hour,Minute,Second}}}) ->
	<<(format_element(hour,Hour))/binary,
		(format_element(min,Minute))/binary,
		(format_element(sec,Second))/binary>>;

format({iso8601,"DD-MM-YYYY HH:MM:SS"},{date_tuple,{{Year,Month,Day},{Hour,Minute,Second}}}) ->
	<<(format_element(day,Day))/binary,("-")/utf8,
		(format_element(month,Month))/binary,("-")/utf8,
		(format_element(year,Year))/binary,(" ")/utf8,
		(format_element(hour,Hour))/binary,(":")/utf8,
		(format_element(min,Minute))/binary,(":")/utf8,
		(format_element(sec,Second))/binary>>;
format({iso8601,"DD/MM/YYYY HH:MM:SS"},{date_tuple,{{Year,Month,Day},{Hour,Minute,Second}}}) ->
	<<(format_element(day,Day))/binary,("/")/utf8,
		(format_element(month,Month))/binary,("/")/utf8,
		(format_element(year,Year))/binary,(" ")/utf8,
		(format_element(hour,Hour))/binary,(":")/utf8,
		(format_element(min,Minute))/binary,(":")/utf8,
		(format_element(sec,Second))/binary>>;
format({iso8601,"DD Month YYYY HH:MM:SS"},{date_tuple,{{Year,Month,Day},{Hour,Minute,Second}}}) ->
	<<(format_element(day,Day))/binary,(" ")/utf8,
		(month(Month,full))/binary,(" ")/utf8,
		(format_element(year,Year))/binary,(" ")/utf8,
		(format_element(hour,Hour))/binary,(":")/utf8,
		(format_element(min,Minute))/binary,(":")/utf8,
		(format_element(sec,Second))/binary>>;
format({iso8601,"DD Month YYYY"},{date_tuple,{{Year,Month,Day},{_,_,_}}}) ->
	<<(format_element(day,Day))/binary,(" ")/utf8,
		(month(Month,full))/binary,(" ")/utf8,
		(format_element(year,Year))/binary>>;

format(_,_) -> a:error(?FUNCTION_NAME(),a000).


%%-----------------------------------
%% @doc Return formated day
-spec format_element(Measure,Value) -> byte() | {error,_Reason}
	when
		Measure :: atom(),
		Value :: integer().

format_element(Measure,0)
	when Measure == hour; Measure == min; Measure == sec -> <<"00">>;
format_element(Measure,1)
	when Measure == hour; Measure == min; Measure == sec; Measure == month; Measure == day -> <<"01">>;
format_element(Measure,2)
	when Measure == hour; Measure == min; Measure == sec; Measure == month; Measure == day -> <<"02">>;
format_element(Measure,3)
	when Measure == hour; Measure == min; Measure == sec; Measure == month; Measure == day -> <<"03">>;
format_element(Measure,4)
	when Measure == hour; Measure == min; Measure == sec; Measure == month; Measure == day -> <<"04">>;
format_element(Measure,5)
	when Measure == hour; Measure == min; Measure == sec; Measure == month; Measure == day -> <<"05">>;
format_element(Measure,6)
	when Measure == hour; Measure == min; Measure == sec; Measure == month; Measure == day -> <<"06">>;
format_element(Measure,7)
	when Measure == hour; Measure == min; Measure == sec; Measure == month; Measure == day -> <<"07">>;
format_element(Measure,8)
	when Measure == hour; Measure == min; Measure == sec; Measure == month; Measure == day -> <<"08">>;
format_element(Measure,9)
	when Measure == hour; Measure == min; Measure == sec; Measure == month; Measure == day -> <<"09">>;

format_element(day,Day) when is_integer(Day) == true, Day >9, Day =< 31 ->
	<<(integer_to_binary(Day))/binary>>;
format_element(day,_) -> a:error(?FUNCTION_NAME(),a009);

format_element(month,Month) when is_integer(Month) == true, Month > 9, Month =< 12 ->
	<<(integer_to_binary(Month))/binary>>;
format_element(month,_) -> a:error(?FUNCTION_NAME(),a009);

format_element(hour,Hours) when is_integer(Hours) == true, Hours > 9, Hours =< 23 ->
	<<(integer_to_binary(Hours))/binary>>;
format_element(hour,_) -> a:error(?FUNCTION_NAME(),a009);

format_element(min,Minutes) when is_integer(Minutes) == true, Minutes > 9, Minutes =< 59 ->
	<<(integer_to_binary(Minutes))/binary>>;
format_element(min,_) -> a:error(?FUNCTION_NAME(),a009);

format_element(sec,Seconds) when is_integer(Seconds) == true, Seconds > 9, Seconds =< 59 ->
	<<(integer_to_binary(Seconds))/binary>>;
format_element(sec,_) -> a:error(?FUNCTION_NAME(),a009);

format_element(year,Year) -> a:to_binary(Year);
format_element(year_short,Year) -> binary:part(integer_to_binary(Year),2,2);

format_element(_,_) -> a:error(?FUNCTION_NAME(),a000).


%%-----------------------------------
%% @doc Return timestamp/data()/seconds from formated time
-spec from_formated(Format_type,Time_source,Output_type) -> tuple() | integer() | false | {error,_Reason}
	when
		Format_type :: atom(),
		Time_source :: string() | unicode:latin1_binary() | tuple(),
		Output_type :: tuple | seconds | timestamp.

from_formated(Format_type,Time_source,Output_type) when is_list(Time_source) ->
	case io_lib:char_list(Time_source) of
		true -> from_formated(Format_type,unicode:characters_to_binary(Time_source),Output_type);
		_ -> a:error(?FUNCTION_NAME(),a000)
	end;
from_formated(ansi,Time_source,Output_type) when is_binary(Time_source) ->
	Pattern = <<"^([A-Za-z]{3}) ([A-Za-z]{3}) ([0-9]{2}) ([0-9]{2})\:([0-9]{2})\:([0-9]{2}) ([0-9]{4})$">>,
	case re:split(Time_source,Pattern,[{return,list}]) of
		[_,_,Month,Day,Hours,Minutes,Seconds,Year,_] ->
			from_formated(date_tuple,{{year(to_integer,Year),month(Month),day(Day)},
				{hour(Hours),minute(Minutes),second(Seconds)}},Output_type);
		_ -> false
	end;
from_formated(rfc850,Time_source,Output_type) when is_binary(Time_source) ->
	Pattern = <<"^[a-zA-Z]{0,10}\, ([0-9]{2})\-([a-zA-Z]{3})\-([0-9]{2}) ([0-9]{2})\:([0-9]{2})\:([0-9]{2}) GMT$">>,
	case re:split(Time_source,Pattern,[{return,list}]) of
		[[],Day,Month,Year,Hours,Minutes,Seconds,[]] ->
			from_formated(date_tuple,{{year(to_integer,Year)+2000,month(Month),day(Day)},
				{hour(Hours),minute(Minutes),second(Seconds)}},Output_type);
		_ -> false
	end;
from_formated(rfc822,Time_source,Output_type) when is_binary(Time_source) ->
	Pattern = <<"^[a-zA-Z]{3}\, ([0-9]{2}) ([a-zA-Z]{3}) ([0-9]{4}) ([0-9]{2})\:([0-9]{2})\:([0-9]{2}) GMT$">>,
	case re:split(Time_source,Pattern,[{return,list}]) of
		[_,Day,Month,Year,Hours,Minutes,Seconds,_] ->
			from_formated(date_tuple,{{year(to_integer,Year),month(Month),day(Day)},
				{hour(Hours),minute(Minutes),second(Seconds)}},Output_type);
		_ -> false
	end;
from_formated(date_tuple,{{Year,Month,Day},{Hours,Minutes,Seconds}},Output_type) ->
	case Output_type of
		tuple -> {{Year,Month,Day},{Hours,Minutes,Seconds}};
		seconds -> calendar:datetime_to_gregorian_seconds({{Year,Month,Day},{Hours,Minutes,Seconds}});
		timestamp -> to_timestamp({{Year,Month,Day},{Hours,Minutes,Seconds}});
		_ -> a:error(?FUNCTION_NAME(),a012)
	end;
from_formated(_,_,_) -> a:error(?FUNCTION_NAME(),a000).