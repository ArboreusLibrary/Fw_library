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
-vsn("0.0.1.144").


%% Module API
-export([str/1]).
-export([error/2]).
-export([bin/1]).
-export([read_file/1]).
-export([cwd/0]).

%% System include

%% Module Include Start
-include("a.hrl").
-include("error_codes.hrl").
%% Module Include End

%%-----------------------------------
%% @spec str(Bitstring::byte()) -> string()
%% @doc Return string converted from binary
-spec str(Bitstring::byte()) -> string().

str(Bitstring) when is_bitstring(Bitstring) -> binary_to_list(Bitstring);
str(_) -> a:error(?FUNCTION_NAME(),a002).

%%-----------------------------------
%% @spec bin(Value) -> binary()
%% where
%%      Value = integer() | string()
%% @doc Return binary within converted value, purposed for integer() or string() datatypes
-spec bin(Value) -> byte() | {error,_Reason} when
	Value :: integer() | string().

bin(Value) when is_list(Value) ->
	case io_lib:char_list(Value) of
		true -> unicode:characters_to_binary(Value);
		_ -> a:error(?FUNCTION_NAME(),a013)
	end;
bin(Value) when is_atom(Value) -> atom_to_binary(Value,utf8);
bin(Value) when is_integer(Value) -> integer_to_binary(Value);
bin(Value) when is_binary(Value) -> Value;
bin(_) -> a:error(?FUNCTION_NAME(),a013).

%%-----------------------------------
%% @spec error(Function_name::tuple(),Error_code::string()) -> tuple()
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