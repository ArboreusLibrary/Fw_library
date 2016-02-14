%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV (http://alexandr.kirilov.me)
%%% @copyright (C) 2015, Arboreus, (http://arboreus.systems)
%%% @doc
%%%
%%% @end
%%% Created : 08. Aug 2015 2:50
%%%-------------------------------------------------------------------
-module(a).
-author("Alexandr KIRILOV (http://alexandr.kirilov.me)").
-vsn("0.0.4.221").


%% Module API
-export([
	error/2,
	to_string/1,
	to_binary/1,
	to_integer/1,
	read_file/1,
	cwd/0,
	var_dump/2,
	test/0
]).

%% System include

%% Module Include Start
-include("a.hrl").
-include("error_codes.hrl").
%% Module Include End

%%-----------------------------------
%% @doc Test application usage
-spec test() -> ok.

test() -> ok.


%%-----------------------------------
%% @doc Write to file the variable value
-spec var_dump(Path,Variable) -> ok | {error, _Reason}
	when
		Path :: unicode:chardata(),
		Variable :: any().

var_dump(Path,Variable) ->
	file:write_file(Path,io_lib:fwrite("~p.\n",[Variable])).


%%-----------------------------------
%% @doc Return string converted from binary
-spec to_string(Bitstring::any()) -> string().

to_string(String) when is_list(String) ->
	case io_lib:char_list(String) of
		true -> String;
		_ -> a:error(?FUNCTION_NAME(),a013)
	end;
to_string(Binary) when is_binary(Binary) -> unicode:characters_to_list(Binary);
to_string(Atom) when is_atom(Atom) -> atom_to_list(Atom);
to_string(Integer) when is_integer(Integer) -> integer_to_list(Integer);
to_string(Float) when is_float(Float) -> float_to_list(Float);
to_string(_) -> a:error(?FUNCTION_NAME(),a013).


%%-----------------------------------
%% @doc Return binary within converted value, purposed for integer() or string() datatypes
-spec to_binary(Value::any()) -> byte() | {error,_Reason}.

to_binary(String) when is_list(String) ->
	case io_lib:char_list(String) of
		true -> unicode:characters_to_binary(String);
		_ -> a:error(?FUNCTION_NAME(),a013)
	end;
to_binary(Binary) when is_binary(Binary) -> Binary;
to_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom,utf8);
to_binary(Integer) when is_integer(Integer) -> integer_to_binary(Integer);
to_binary(Float) when is_float(Float) -> float_to_binary(Float);
to_binary(_) -> a:error(?FUNCTION_NAME(),a013).


%%-----------------------------------
%% @doc Return integer
-spec to_integer(Value::any()) -> integer() | {error,_Reason}.

to_integer(String) when is_list(String) ->
	case io_lib:char_list(String) of
		true -> list_to_integer(String);
		_ -> a:error(?FUNCTION_NAME(),a013)
	end;
to_integer(Integer) when is_integer(Integer) -> Integer;
to_integer(Float) when is_float(Float) -> list_to_integer(float_to_list(Float,[{decimals,0}]));
to_integer(Binary) when is_binary(Binary) -> binary_to_integer(Binary);
to_integer(_) -> a:error(?FUNCTION_NAME(),a013).


%%-----------------------------------
%% @doc Return a tuple within function Id and the reason of an error
-spec error({_,{Module,Function,Arity}},Error_code::atom()) -> tuple()
	when Module::atom(), Function::atom(), Arity::integer().

error({_,{Module,Function,Arity}},Error_code) when is_atom(Error_code) ->
	Reason = proplists:get_value(Error_code,?ERROR_CODES),
	case Reason of
		undefined ->
			{error,{a,error,2},proplists:get_value(e001,?ERROR_CODES)};
		_ ->
			{error,{Module,Function,Arity,Reason}}
	end;
error(_,_) -> {error,{a,error,2,proplists:get_value(e000,?ERROR_CODES)}}.


%%-----------------------------------
%% @spec read_file(Path) -> byte() | {error,_}
%% where
%%      Path = string()
%% @doc Return the requested file in binary mode or {error,Reason}
-spec read_file(Path::string()) -> byte() | {error,_}.

read_file(Path) when is_list(Path) ->
	case io_lib:char_list(Path) of
		true ->
			case file:read_file(Path) of
				{ok,File} -> File;
				{error,Reason} -> {error,Reason}
			end;
		false -> a:error(?FUNCTION_NAME(),a002)
	end;
read_file(_) -> a:error(?FUNCTION_NAME(),a002).


%%-----------------------------------
%% @spec cwd() -> string() | {errror,Reason}
%% where
%%      Dir = string()
%% @doc Return a working directory
-spec cwd() -> string() | {errror,_}.

cwd() ->
	case file:get_cwd() of
		{ok,Dir} -> Dir;
		{error,Reason} -> {error,Reason}
end.