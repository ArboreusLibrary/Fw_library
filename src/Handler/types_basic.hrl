%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Data types for Arboreus library: Basic types
%%%
%%% @end
%%% Created : 07. Янв. 2018 22:22
%%%-------------------------------------------------------------------
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").
-vsn("389").

%% ------------------------------------------
%% Basic types
%% ------------------------------------------

-type unix_path() :: string().
-type unix_path_binary() :: unicode:unicode_binary().

-type a_byte_8() :: 0..255.
-type a_byte_16() :: 0..16#ffffff.

-type year() :: 0..9999.
-type year_short() :: 0..99.
-type month() :: 1..12.
-type day() :: 1..31.
-type hour() :: 0..23.
-type minute() :: 0..59.
-type second() :: 0..59.
