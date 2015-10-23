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
-vsn("0.0.1.144").

%% Module API
-export([check/3]).
-export([check_parameters/2]).

%% System include

%% Module Include Start
-include("../Handler/a.hrl").
%% Module Include End

%%-----------------------------------
%% @spec check(Type,Parameter,Type_properties) -> nomatch | {match,Checked_parameter}
%% where
%%      Type::atom()
%%      Parameter::string()
%%      Type_properties::list()
%% @doc Checking the request parameter through the Regexp defined for the type. Return the
%% Parameter = Checked_parameter() converted to the defined datatype from list
-spec check(Type,Parameter,Type_properties) -> nomatch | {match,_Checked_parameter}
	when Type::atom(), Parameter::string(), Type_properties::list().

check(integer,Parameter,_) when is_list(Parameter) == true ->
	case io_lib:char_list(Parameter) of
		true ->
			Pattern = "^[0-9]*$",
			case re:run(Parameter,Pattern) of
				nomatch -> nomatch;
				{match,_} -> list_to_integer(Parameter)
			end;
		_ ->
			a:error(?FUNCTION_NAME(),a014)
	end;
check(atom,Parameter,_) when is_list(Parameter) == true ->
	case io_lib:char_list(Parameter) of
		true ->
			Pattern = "^[a-z]{1}[a-zA-Z0-9\_]*$",
			case re:run(Parameter,Pattern) of
				nomatch -> nomatch;
				{match,_} -> list_to_atom(Parameter)
			end;
		_ ->
			a:error(?FUNCTION_NAME(),a014)
	end;
check(boolean,Parameter,_) when is_list(Parameter) ->
	case io_lib:char_list(Parameter) of
		true ->
			Pattern = "^true$|^false$",
			case re:run(Parameter,Pattern) of
				nomatch -> nomatch;
				{match,_} ->
					if
						Parameter == "true" -> true;
						Parameter == "false" -> false
					end
			end;
		_ ->
			a:error(?FUNCTION_NAME(),a014)
	end;
check(e_mail,Parameter,_) when is_list(Parameter) == true ->
	case io_lib:char_list(Parameter) of
		true ->
			Pattern = "^[A-Za-z0-9](([_\.\-]?[a-zA-Z0-9]+)*)@([A-Za-z0-9]+)(([\.\-]?[a-zA-Z0-9]+)*)\.([A-Za-z]{2,})$",
			Pattern_unicode = unicode:characters_to_binary(Pattern),
			Parameter_binary = unicode:characters_to_binary(Parameter),
			case re:run(Parameter_binary,Pattern_unicode) of
				nomatch -> nomatch;
				{match,_} -> unicode:characters_to_binary(Parameter)
			end;
		_ ->
			a:error(?FUNCTION_NAME(),a014)
	end;
check(id,Parameter,[Length,Output])
	when
		is_list(Parameter) == true,
		is_integer(Length) == true ->
	case io_lib:char_list(Parameter) of
		true ->
			Pattern = lists:concat(["^[a-zA-Z0-9]{",integer_to_list(Length),"}$"]),
			Pattern_unicode = unicode:characters_to_binary(Pattern),
			Parameter_binary = unicode:characters_to_binary(Parameter),
			case re:run(Parameter_binary,Pattern_unicode) of
				nomatch -> nomatch;
				{match,_} ->
					case Output of
						binary -> Parameter_binary;
						string -> binary_to_list(Parameter_binary);
						_ -> a:error(?FUNCTION_NAME(),a012)
					end
			end;
		_ ->
			a:error(?FUNCTION_NAME(),a014)
	end;
check(Type,_,_) when is_atom(Type) == true -> a:error(?FUNCTION_NAME(),m003_001);
check(_,_,_) -> a:error(?FUNCTION_NAME(),a000).

%% ----------------------------
%% @spec check_parameters(Data_schema,Parameters) -> list() | false.
%% @doc Checking requested parameters in following of Data_schema selected
%% in following of table name.
%% Return:
%%      false - in case of wrong request parameters
%%      list() - in case of passed checking
%% Example of passed checking: [{parameter1,"Value1"},{parameter2,"Value2"},{parameter2,"Value2"}]
-spec check_parameters(Data_schema,Parameters) -> list() | false
	when Data_schema::list(), Parameters::list().

check_parameters(Data_schema,Parameters) ->
	check_parameters(Data_schema,Parameters,[]).

check_parameters([],_,Result) -> Result;
check_parameters([Rule|Data_schema],Parameters,Result) ->
	{Parameter_name,Parameter_properties} = Rule,
	Check_value = fun(Parameter_value) ->
		Type = proplists:get_value(type,Parameter_properties),
		Type_properties = proplists:get_value(type_properties,Parameter_properties),
		Parameter_checked = a_params:check(Type,Parameter_value,Type_properties),
		case Parameter_checked of
			nomatch ->
				false;
			{error,Reason} ->
				{error,Reason};
			_ ->
				Name = list_to_atom(Parameter_name),
				Result_out = lists:append(Result,[{Name,Parameter_checked}]),
				check_parameters(Data_schema,Parameters,Result_out)
		end
	end,
	case proplists:get_value(require,Parameter_properties) of
		true ->
			Parameter_value = proplists:get_value(Parameter_name,Parameters),
			if
				Parameter_value == undefined ->
					false;
				true ->
					Check_value(Parameter_value)
			end;
		false ->
			Parameter_value = proplists:get_value(Parameter_name,Parameters),
			if
				Parameter_value == undefined ->
					Parameter_default = proplists:get_value(require_default,Parameter_properties),
					Check_value(Parameter_default);
				true ->
					Check_value(Parameter_value)
			end
	end.