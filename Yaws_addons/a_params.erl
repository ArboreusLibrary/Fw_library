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
-spec check(Type,Parameter,Type_properties) -> nomatch | _Checked_parameter
	when Type::atom(), Parameter::string(), Type_properties::list().

%% Float, regex rule ^[\-]?[0-9]*\.[0-9]*$
check(float,Parameter,_) when is_list(Parameter) ->
	case io_lib:char_list(Parameter) of
		true ->
			try list_to_float(Parameter)
			catch _:_ -> nomatch end;
		false ->
			a:error(?FUNCTION_NAME(),a014)
	end;
%% Integer, regex rule ^[\-]?[0-9]*$
check(integer,Parameter,_) when is_list(Parameter) ->
	case io_lib:char_list(Parameter) of
		true ->
			try list_to_integer(Parameter)
			catch _:_ -> nomatch end;
		false ->
			a:error(?FUNCTION_NAME(),a014)
	end;
%% Positive integer, regex rule "^[0-9]*$"
check(pos_integer,Parameter,_) when is_list(Parameter) ->
	case io_lib:char_list(Parameter) of
		true ->
			case check(integer,Parameter,[]) of
				nomatch -> nomatch;
				Integer ->
					if
						Integer >= 0 -> Integer;
						true -> nomatch
					end
			end;
		false ->
			a:error(?FUNCTION_NAME(),a014)
	end;
%% Neg_integer, regex rule ^[\-]{1}[0-9]*$
check(neg_integer,Parameter,_) ->
	case io_lib:char_list(Parameter) of
		true ->
			case check(integer,Parameter,[]) of
				nomatch -> nomatch;
				Integer ->
					if
						Integer < 0 -> Integer;
						true -> nomatch
					end
			end;
		false ->
			a:error(?FUNCTION_NAME(),a014)
	end;
%% Ranged integer
check(ranged_integer,Parameter,[Minor,Major]) ->
	case io_lib:char_list(Parameter) of
		true ->
			case check(integer,Parameter,[]) of
				nomatch -> nomatch;
				Integer ->
					if
						Integer =< Major ->
							if
								Integer >= Minor -> Integer;
								true -> nomatch
							end;
						true -> nomatch
					end
			end;
		false ->
			a:error(?FUNCTION_NAME(),a014)
	end;
%% Atom
check(atom,Parameter,_) when is_list(Parameter) ->
	case io_lib:char_list(Parameter) of
		true ->
			try list_to_atom(Parameter)
			catch _:_ -> nomatch end;
		_ ->
			a:error(?FUNCTION_NAME(),a014)
	end;
%% A_atom, regex rule ^[a-z]{1}[a-zA-Z0-9\_]*$
check(a_atom,Parameter,_) when is_list(Parameter) ->
	case io_lib:char_list(Parameter) of
		true ->
			Pattern = "^[a-z]{1}[a-zA-Z0-9\_]*$",
			case re:run(Parameter,Pattern) of
				nomatch -> nomatch;
				{match,_} -> list_to_atom(Parameter)
			end
	end;
%% Boolean, regex rule ^true$|^false$
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
%% Latin_name, regex rule ^[a-zA-Z0-9 -_]{1,lenght}$
check(latin_name,Parameter,[Length]) when is_list(Parameter) ->
	case io_lib:char_list(Parameter) of
		true ->
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
		_ ->
			a:error(?FUNCTION_NAME(),a014)
	end;
%% Unicode binary, regex rule ^(<<"){1}((?!">>|[t1j]).){1,}(">>)$
check(unicode_binary,Parameter,[Exception_rule,Length]) when is_list(Parameter) ->
	case io_lib:char_list(Parameter) of
		true ->
			Length_binary = fun() ->
				case Length of
					free -> <<("")/utf8>>;
					Length ->
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
					Exception_rule ->
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
		_ ->
			a:error(?FUNCTION_NAME(),a014)
	end;
%% E-mail
check(e_mail,Parameter,_) when is_list(Parameter) ->
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
%% Id
check(id,Parameter,[Length,Output])
	when
		is_list(Parameter) == true,
		is_integer(Length) == true,
		Length > 0 ->
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