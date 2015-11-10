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
-vsn("0.0.3.158").

%% Module API
-export([check/3]).
-export([check_parameters/2]).
%% System include

%% Module Include Start
-include("../Handler/a.hrl").
%% Module Include End

%%-----------------------------------
%% @spec check(Type,Parameter,Type_properties) -> nomatch | {match,Checked_parameter} | {error,_Reason}
%% where
%%      Type::atom()
%%      Parameter::string()
%%      Type_properties::list()
%% @doc Checking the request parameter through the Regexp defined for the type. Return the
%% Parameter = Checked_parameter() converted to the defined datatype from list
-spec check(Type,Parameter,Type_properties) -> nomatch | _Checked_parameter | {error,_Reason}
	when Type::atom(), Parameter::string(), Type_properties::list().

check(Type,Parameter,Type_properties) ->
	case io_lib:char_list(Parameter) of
		true -> parameter_value(Type,Parameter,Type_properties);
		false -> a:error(?FUNCTION_NAME(),a014)
	end.

%%-----------------------------------
%% @spec parameter_value(Type,Parameter,Type_properties) -> nomatch | {match,Checked_parameter} | {error,_Reason}
%% where
%%      Type::atom()
%%      Parameter::string()
%%      Type_properties::list()
%% @doc Secondary function for check/3
-spec parameter_value(Type,Parameter,Type_properties) -> nomatch | _Checked_parameter | {error,_Reason}
	when Type::atom(), Parameter::string(), Type_properties::list().

%% Float, regex rule ^[\-]?[0-9]*\.[0-9]*$
parameter_value(float,Parameter,_) ->
	try list_to_float(Parameter)
	catch _:_ -> nomatch end;
%% Integer, regex rule ^[\-]?[0-9]*$
parameter_value(integer,Parameter,_) ->
	try list_to_integer(Parameter)
	catch _:_ -> nomatch end;
%% Positive integer, regex rule "^[0-9]*$"
parameter_value(pos_integer,Parameter,_) ->
	case check(integer,Parameter,[]) of
		nomatch -> nomatch;
		Integer ->
			if
				Integer >= 0 -> Integer;
				true -> nomatch
			end
	end;
%% Neg_integer, regex rule ^[\-]{1}[0-9]*$
parameter_value(neg_integer,Parameter,_) ->
	case check(integer,Parameter,[]) of
		nomatch -> nomatch;
		Integer ->
			if
				Integer < 0 -> Integer;
				true -> nomatch
			end
	end;
%% Ranged integer
parameter_value(ranged_integer,Parameter,[Minor,Major]) ->
	case check(integer,Parameter,[]) of
		nomatch -> nomatch;
		Integer ->
			if
				Minor < Major ->
					if
						Integer =< Major ->
							if
								Integer >= Minor -> Integer;
								true -> nomatch
							end;
						true -> nomatch
					end;
				true -> a:error(?FUNCTION_NAME(),a000)
			end
	end;
%% Atom
parameter_value(atom,Parameter,_) ->
	try list_to_atom(Parameter)
	catch _:_ -> nomatch end;
%% A_atom, regex rule ^[a-z]{1}[a-zA-Z0-9\_]*$
parameter_value(a_atom,Parameter,_) ->
	Pattern = "^[a-z]{1}[a-zA-Z0-9\_]*$",
	case re:run(Parameter,Pattern) of
		nomatch -> nomatch;
		{match,_} -> list_to_atom(Parameter)
	end;
%% Boolean, regex rule ^true$|^false$
parameter_value(boolean,Parameter,_) ->
	Pattern = "^true$|^false$",
	case re:run(Parameter,Pattern) of
		nomatch -> nomatch;
		{match,_} ->
			if
				Parameter == "true" -> true;
				Parameter == "false" -> false
			end
	end;
%% Latin_name, regex rule ^[a-zA-Z0-9 -_]{1,lenght}$
parameter_value(latin_name,Parameter,[Length]) ->
	Pattern = fun() ->
		case Length of
			free -> <<("^[a-zA-Z0-9 \-\_]{1,}$")/utf8>>;
			_ -> <<("^[a-zA-Z0-9 \-\_]{1,")/utf8,(integer_to_binary(Length))/binary,("}$")/utf8>>
		end
	          end,
	Binary_parameter = unicode:characters_to_binary(Parameter),
	case re:run(Binary_parameter,Pattern()) of
		nomatch -> nomatch;
		{match,_} -> Binary_parameter
	end;
%% Unicode binary, regex rule ^(<<"){1}((?!">>|[t1j]).){1,}(">>)$
parameter_value(unicode_binary,Parameter,[Exception_rule,Length]) ->
	Length_binary = fun() ->
		case Length of
			free -> <<("")/utf8>>;
			_ ->
				if
					is_integer(Length) ->
						if
							Length >=2 -> integer_to_binary(Length) ;
							true -> false
						end;
					true -> false
				end
		end
	                end,
	Exception_binary = fun() ->
		case Exception_rule of
			"" -> <<("")/utf8>>;
			free -> <<("")/utf8>>;
			_ ->
				Test_rule = io_lib:char_list(Exception_rule),
				if
					Test_rule == false -> false;
					true ->
						Binary_rule = unicode:characters_to_binary(Exception_rule),
						<<("|[")/utf8,Binary_rule/binary,("]")/utf8>>
				end
		end
	                   end,
	case Length_binary() of
		false -> a:error(?FUNCTION_NAME(),a014);
		_ ->
			case Exception_binary() of
				false -> a:error(?FUNCTION_NAME(),a014);
				_ ->
					Pattern = fun() ->
						<<("^(<<\"){1}((?!\">>")/utf8,
							(Exception_binary())/binary,
							(").){1,")/utf8,
							(Length_binary())/binary,
							("}(\">>)$")/utf8>>
					          end,
					case re:run(Parameter,Pattern()) of
						nomatch -> nomatch;
						{match,_} ->
							Parameter_binary = unicode:characters_to_binary(Parameter),
							Size = byte_size(Parameter_binary),
							binary:part(Parameter_binary,3,Size-6)
					end
			end
	end;
%% Id
parameter_value(id,Parameter,[Length,Output]) ->
	case is_integer(Length) of
		false -> a:error(?FUNCTION_NAME(),a000);
		_ ->
			if
				Length > 0 ->
					Pattern = <<("^[a-zA-Z0-9]{")/utf8,
						(integer_to_binary(Length))/binary,
						("}$")/utf8>>,
					Parameter_binary = unicode:characters_to_binary(Parameter),
					case re:run(Parameter_binary,Pattern) of
						nomatch -> nomatch;
						{match,_} ->
							case Output of
								binary -> Parameter_binary;
								string -> binary_to_list(Parameter_binary);
								_ -> a:error(?FUNCTION_NAME(),a012)
							end
					end;
				true -> a:error(?FUNCTION_NAME(),a000)
			end
	end;
parameter_value(Type,_,_) when is_atom(Type) -> a:error(?FUNCTION_NAME(),m003_001);
parameter_value(_,_,_) -> a:error(?FUNCTION_NAME(),a000).


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