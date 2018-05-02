%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc The data types definitions
%%%
%%% @end
%%% Created : 10. Апр. 2018 14:32
%%%-------------------------------------------------------------------
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").


-type unix_path() :: unix_path_binary() | unix_path_string().
-type unix_path_string() :: unicode:charlist().
-type unix_path_binary() :: unicode:unicode_binary().

-type utf_text() :: utf_text_binary() | utf_text_string().
-type utf_text_string() :: unicode:charlist().
-type utf_text_binary() :: unicode:unicode_binary().

-type md5() :: md5_binary() | md5_string().
-type md5_binary() :: <<_:32>>.
-type md5_string() :: utf_text_string().

-type record() :: tuple().
-type state() :: term().

-type list_of_properties() :: [any()].
-type list_of_records() :: [record()].
-type list_of_integers() :: [integer()].
-type list_of_atoms() :: [atom()].

-type a_byte_8() :: 0..255.
-type a_byte_16() :: 0..16#ffffff.

-type year() :: 0..9999.
-type year_short() :: 0..99.
-type month() :: 1..12.
-type day() :: 1..31.
-type hour() :: 0..23.
-type minute() :: 0..59.
-type second() :: 0..59.

-type process_id() :: any().