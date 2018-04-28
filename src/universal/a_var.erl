%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc The variables handler
%%%
%%% @end
%%% Created : 10. Апр. 2018 14:40
%%%-------------------------------------------------------------------
-module(a_var).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% System include
-include("../data_models/types/types_general.hrl").

%% API
-export([
	test/0,
	dump/2,
	to_string/1,
	to_binary/1,
	to_integer/1
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%%-----------------------------------
%% @doc Write to file the variable value
-spec dump(Path,Variable) -> ok | {error, _Reason}
	when
	Path :: unix_path_string(),
	Variable :: any().

dump(Path,Variable) ->
	file:write_file(Path,io_lib:fwrite("~p.\n",[Variable])).


%%-----------------------------------
%% @doc Return string converted from binary
-spec to_string(Any::any()) -> utf_text().

to_string(String) when is_list(String) -> String;
to_string(Binary) when is_binary(Binary) -> unicode:characters_to_list(Binary);
to_string(Atom) when is_atom(Atom) -> atom_to_list(Atom);
to_string(Integer) when is_integer(Integer) -> integer_to_list(Integer);
to_string(Float) when is_float(Float) -> float_to_list(Float).


%%-----------------------------------
%% @doc Return binary within converted value, purposed for integer() or string() datatypes
-spec to_binary(Any::any()) -> byte().

to_binary(String) when is_list(String) -> unicode:characters_to_binary(String);
to_binary(Binary) when is_binary(Binary) -> Binary;
to_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom,utf8);
to_binary(Integer) when is_integer(Integer) -> integer_to_binary(Integer);
to_binary(Float) when is_float(Float) -> float_to_binary(Float).


%%-----------------------------------
%% @doc Return integer
-spec to_integer(Any::any()) -> integer().

to_integer(String) when is_list(String) -> list_to_integer(String);
to_integer(Integer) when is_integer(Integer) -> Integer;
to_integer(Float) when is_float(Float) -> list_to_integer(float_to_list(Float,[{decimals,0}]));
to_integer(Binary) when is_binary(Binary) -> binary_to_integer(Binary).