%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV (http://alexandr.kirilov.me)
%%% @copyright (C) 2015, Arboreus, (http://arboreus.systems)
%%% @doc
%%%
%%% @end
%%% Created : 06. Sep 2015 0:18
%%%-------------------------------------------------------------------
-module(a_params).
-author("Alexandr KIRILOV (http://alexandr.kirilov.me)").
-vsn("0.0.0.0").

%% Module API
-export([check/1]).

%% System include

%% Module Include Start
-include("../Handler/a.hrl").
%% Module Include End

%% @spec check({Type,Parameter}) -> Checked_parameter()
%% where
%%      Type == atom()
%%      Parameters = Parameter()
%% @doc Checking the request parameter through the Regexp defined for the type. Return the
%% Parameter = Checked_parameter() converted to the defined datatype from list
check({integer,Parameter}) when is_list(Parameter) == true ->
	Pattern = "^[0-9]*$",
	case re:run(Parameter,Pattern) of
		nomatch -> nomatch;
		{match,_} -> list_to_integer(Parameter)
	end;
check({atom,Parameter}) when is_list(Parameter) == true ->
	Pattern = "^[a-z]{1}[a-zA-Z0-9\_]*$",
	case re:run(Parameter,Pattern) of
		nomatch -> nomatch;
		{match,_} -> list_to_atom(Parameter)
	end;
check({boolean,Parameter}) when is_list(Parameter) ->
	Pattern = "^true$|^false$|^yes$|^no$|^1$|^0$",
	case re:run(Parameter,Pattern) of
		nomatch -> nomatch;
		{match,_} ->
			if
				Parameter == "true" -> true;
				Parameter == "false" -> false;
				Parameter == "yes" -> yes;
				Parameter == "no" -> no;
				Parameter == "1" -> 1;
				Parameter == "0" -> 0
			end
	end;
check({e_mail,Parameter}) when is_list(Parameter) == true ->
	Pattern = "^[A-Za-z0-9](([_\.\-]?[a-zA-Z0-9]+)*)@([A-Za-z0-9]+)(([\.\-]?[a-zA-Z0-9]+)*)\.([A-Za-z]{2,})$",
	case re:run(Parameter,Pattern) of
		nomatch -> nomatch;
		{match,_} -> Parameter
	end;
check({id,[Parameter,Length,Output]})
	when
		is_list(Parameter) == true,
		is_integer(Length) == true ->
	Pattern = lists:concat(["^[a-zA-Z0-9]{",integer_to_list(Length),"}$"]),
	case re:run(Parameter,Pattern) of
		nomatch -> nomatch;
		{match,_} ->
			case Output of
				binary -> list_to_binary(Parameter);
				string -> Parameter;
				_ -> a:error(?FUNCTION_NAME(),a012)
			end
	end;
check({Type,_}) when is_atom(Type) == true -> a:error(?FUNCTION_NAME(),m003_001);
check(_) -> a:error(?FUNCTION_NAME(),a000).