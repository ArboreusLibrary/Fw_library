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
-vsn("0.0.1.92").


%% Module API
-export([str/1]).
-export([error/2]).
-export([bin/1]).

%% System include

%% Module Include Start
-include("a.hrl").
-include("error_codes.hrl").
%% Module Include End

%% @spec str(Function::bitstring()) -> string()
%% @doc Return string converted from binary
str(Function) when is_bitstring(Function) == true -> binary_to_list(Function);
str(_) -> a:error(?FUNCTION_NAME(),a002).

%% @spec bin(Function) -> binary()
%% where
%%      Function = integer() | string()
%% @doc Return binary within converted value, purposed for integer() or string() datatypes
bin(Function) when is_integer(Function) == true -> integer_to_binary(Function);
bin(Function) when is_list(Function) == true -> list_to_binary(Function);
bin(_) -> a:error(?FUNCTION_NAME(),a013).

%% @spec error(Function_name::tuple(),Reason::string()) -> tuple()
%% @doc Return a tuple within function Id and the reason of an error
error({_,{Module,Function,Arity}},Error_code) when is_atom(Error_code) ->
	Reason = proplists:get_value(Error_code,?ERROR_CODES),
	case Reason of
		undefined ->
			{error,{a,error,2},proplists:get_value(e001,?ERROR_CODES)};
		_ ->
			{error,{Module,Function,Arity,Reason}}
	end;
error(_,_) -> {error,{a,error,2,proplists:get_value(e000,?ERROR_CODES)}}.