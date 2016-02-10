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
-vsn("0.0.13.198").

%% Module API
-export([
	check/3,
	check_parameters/2
]).

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

check(user_defined,Parameter,[Module,Function,Arguments]) ->
	try apply(Module,Function,[Parameter,Arguments])
	catch _:_ -> a:error(?FUNCTION_NAME(),m003_002) end;
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
%% Float, regex rule ^[0-9]*\.[0-9]*$
parameter_value(pos_float,Parameter,_) ->
	case check(float,Parameter,[]) of
		nomatch -> nomatch;
		Value ->
			if
				Value >= 0 -> Value;
				true -> nomatch
			end
	end;
%% Negative float
parameter_value(neg_float,Parameter,_) ->
	case check(float,Parameter,[]) of
		nomatch -> nomatch;
		Value ->
			if
				Value < 0 -> Value;
				true -> nomatch
			end
	end;
%% Float from list
parameter_value(float_from_list,Parameter,[List]) ->
	case parameter_value(float,Parameter,[]) of
		nomatch -> nomatch;
		Float ->
			case lists:member(Float,List) of
				true -> Float;
				false -> nomatch
			end
	end;
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
%% Integer from list
parameter_value(integer_from_list,Parameter,[List]) ->
	case parameter_value(integer,Parameter,[]) of
		nomatch -> nomatch;
		Integer ->
			case lists:member(Integer,List) of
				true -> Integer;
				false -> nomatch
			end
	end;
%% Atom
parameter_value(atom,Parameter,_) ->
	try list_to_atom(Parameter)
	catch _:_ -> nomatch end;
%% Atom from list
parameter_value(atom_from_list,Parameter,[List]) ->
	case parameter_value(atom,Parameter,[]) of
		nomatch -> nomatch;
		Atom ->
			case lists:member(Atom,List) of
				true -> Atom;
				false -> nomatch
			end
	end;
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
			free ->
				<<("^(<<\"){1}([a-zA-Z0-9 _-]{1,})(\">>)$")/utf8>>;
			_ ->
				<<("^(<<\"){1}([a-zA-Z0-9 \-\_]{1,")/utf8,
				(integer_to_binary(Length))/binary,
				("})(\">>)$")/utf8>>
		end
	          end,
	Binary_parameter = unicode:characters_to_binary(Parameter),
	case re:run(Binary_parameter,Pattern()) of
		nomatch -> nomatch;
		{match,_} ->
			Size = byte_size(Binary_parameter),
			binary:part(Binary_parameter,3,Size-6)
	end;

%% Unicode_base64 ^([a-zA-Z0-9\=\+\/]{4,})$
parameter_value(base64,Parameter,[az_esm]) ->
	Pattern = "^([a-zA-Z0-9\=\+\/]{4,})$",
	try
		case re:run(Parameter,Pattern) of
			nomatch -> nomatch;
			{match,_} -> Parameter
		end
	catch _:_ -> nomatch
	end;
parameter_value(base64,Parameter,[az_esm_binary]) ->
	case parameter_value(base64,Parameter,[az_esm]) of
		nomatch -> nomatch;
		_ -> unicode:characters_to_binary(Parameter)
	end;
parameter_value(base64,Parameter,[az_esm_unicode,string]) ->
	case parameter_value(base64,Parameter,[az_esm]) of
		nomatch -> nomatch;
		_ ->
			try binary_to_list(base64:decode(Parameter))
			catch _:_ -> nomatch
			end
	end;
parameter_value(base64,Parameter,[az_esm_unicode,binary]) ->
	case parameter_value(base64,Parameter,[az_esm_binary]) of
		nomatch -> nomatch;
		_ ->
			try base64:decode(Parameter)
			catch _:_  -> nomatch
			end
	end;
parameter_value(base64,Parameter,[typified,Type,Type_properties]) ->
	case parameter_value(base64,Parameter,[az_esm_unicode,string]) of
		nomatch -> nomatch;
		Checked_parameter -> check(Type,Checked_parameter,Type_properties)
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
%% IP regex rule ^([\d]{1,3})\.([\d]{1,3})\.([\d]{1,3})\.([\d]{1,3})$
parameter_value(ipv4,Parameter,[Output_type]) ->
	try
		{ok,Ip_tuple} = inet:parse_ipv4_address(Parameter),
		case Output_type of
			string -> Parameter;
			binary -> unicode:characters_to_binary(Parameter);
			list -> tuple_to_list(Ip_tuple);
			tuple -> Ip_tuple;
			integer -> a_net:ipv4_to_integer(Ip_tuple);
			_ -> a:error(?FUNCTION_NAME(),m003_003)
		end
	catch _:_ -> nomatch end;
%% IP v6 regex rule ^(\:?([a-z0-9]{4})){8}$
parameter_value(ipv6,Parameter,[Output_type]) ->
	try
		{ok,Ip_tuple} = inet:parse_ipv6_address(Parameter),
		case Output_type of
			string -> Parameter;
			binary -> unicode:characters_to_binary(Parameter);
			list -> tuple_to_list(Ip_tuple);
			tuple -> Ip_tuple;
			_ -> a:error(?FUNCTION_NAME(),m003_003)
		end
	catch _:_ -> nomatch end;
%% FQDN regex rule
parameter_value(fqdn,Parameter,[Output_type]) ->
	Pattern = "^(\.?([a-zA-Z0-9\-\_]{1,})){0,}$",
	case re:run(Parameter,Pattern) of
		nomatch -> nomatch;
		{match,_} ->
			case Output_type of
				string -> Parameter;
				binary -> unicode:characters_to_binary(Parameter)
			end
	end;
%% E-mail regex rule ^(\.?([a-zA-Z0-9\-\_]{1,})){1,}\@(\.?([a-zA-Z0-9\-\_]{1,})){1,}$
parameter_value(e_mail,Parameter,[Output_type]) ->
	Pattern = "^(\.?([a-zA-Z0-9\-\_]{1,})){1,}\@(\.?([a-zA-Z0-9\-\_]{1,})){1,}$",
	case re:run(Parameter,Pattern) of
		nomatch -> nomatch;
		{match,_} ->
			case Output_type of
				string -> Parameter;
				binary -> unicode:characters_to_binary(Parameter)
			end
	end;
%% Unicode binary, regex rule ^((?![t1j]).){1,}$
parameter_value(unicode_binary,Parameter,[free]) ->
	unicode:characters_to_binary(Parameter);
parameter_value(unicode_binary,Parameter,[free,free]) ->
	unicode:characters_to_binary(Parameter);
parameter_value(unicode_binary,Parameter,[free,{less_equal,Length}]) ->
	if
		is_integer(Length), Length >= 1 ->
			Binary = unicode:characters_to_binary(Parameter),
			if
				byte_size(Binary) =< Length -> Binary;
				true -> nomatch
			end;
		true -> a:error(?FUNCTION_NAME(),a003)
	end;
parameter_value(unicode_binary,Parameter,[free,{size,Length}]) ->
	if
		is_integer(Length), Length >= 1 ->
			Binary = unicode:characters_to_binary(Parameter),
			if
				byte_size(Binary) == Length -> Binary;
				true -> nomatch
			end;
		true -> a:error(?FUNCTION_NAME(),a003)
	end;
parameter_value(unicode_binary,Parameter,[free,{range,Minor_length,Major_length}]) ->
	if
		is_integer(Minor_length), is_integer(Major_length),
		Minor_length >= 1, Major_length > Minor_length ->
			Binary = unicode:characters_to_binary(Parameter),
			Size = byte_size(Binary),
			if
				Size >= Minor_length, Size =< Major_length -> Binary;
				true -> nomatch
			end;
		true -> a:error(?FUNCTION_NAME(),a003)
	end;
parameter_value(unicode_binary,Parameter,[{except,Exception_chars},Length_type]) ->
	case io_lib:char_list(Exception_chars) of
		true ->
			Binary = unicode:characters_to_binary(Parameter),
			Exception = unicode:characters_to_binary(Exception_chars),
			Pattern = fun() ->
				case Length_type of
					free ->
						<<("^((?![")/utf8,(Exception)/binary,("]).){1,}$")/utf8>>;
					{less_equal,Length} ->
						if
							is_integer(Length), Length >= 2 ->
								<<("^((?![")/utf8,(Exception)/binary,
									("]).){1,")/utf8,(integer_to_binary(Length))/binary,
									("}$")/utf8>>;
							true -> a:error(?FUNCTION_NAME(),a003)
						end;
					{size,Length} ->
						if
							is_integer(Length), Length >= 2 ->
								<<("^((?![")/utf8,(Exception)/binary,
									("]).){")/utf8,(integer_to_binary(Length))/binary,
									("}$")/utf8>>;
							true -> a:error(?FUNCTION_NAME(),a003)
						end;
					{range,Minor_length,Major_length} ->
						if
							is_integer(Minor_length),is_integer(Major_length),
							Minor_length >= 1, Major_length > Minor_length ->
								<<("^((?![")/utf8,(Exception)/binary,
									("]).){")/utf8,(integer_to_binary(Minor_length))/binary,
									(",")/utf8,(integer_to_binary(Major_length))/binary,
									("}$")/utf8>>;
							true -> a:error(?FUNCTION_NAME(),a003)
						end
				end
			end,
			case Pattern() of
				{error,Reason} -> {error,Reason};
				Pattern_binary ->
					case re:run(Binary,Pattern_binary) of
						nomatch -> nomatch;
						{match,_} -> Binary
					end
			end;
		false -> a:error(?FUNCTION_NAME(),a014)
	end;
%% Unicode binary_wrapped, regex rule ^(<<"){1}((?!">>|[t1j]).){1,}(">>)$
parameter_value(unicode_binary_wrapped,Parameter,[free]) ->
	parameter_value(unicode_binary_wrapped,Parameter,[free,free]);
parameter_value(unicode_binary_wrapped,Parameter,[free,free]) ->
	Pattern = <<"^(<<\"){1}((?!\">>).){1,}(\">>)$">>,
	Parameter_binary = unicode:characters_to_binary(Parameter),
	case re:run(Parameter_binary,Pattern) of
		nomatch -> nomatch;
		{match,_} ->
			Size = byte_size(Parameter_binary),
			binary:part(Parameter_binary,3,Size-6)
	end;
parameter_value(unicode_binary_wrapped,Parameter,[free,{less_equal,Length}]) ->
	case parameter_value(unicode_binary_wrapped,Parameter,[free,free]) of
		nomatch -> nomatch;
		Binary_parameter ->
			if
				byte_size(Binary_parameter) =< Length -> Binary_parameter;
				true -> nomatch
			end
	end;
parameter_value(unicode_binary_wrapped,Parameter,[free,{size,Length}]) ->
	case parameter_value(unicode_binary_wrapped,Parameter,[free,free]) of
		nomatch -> nomatch;
		Binary_parameter ->
			if
				byte_size(Binary_parameter) == Length -> Binary_parameter;
				true -> nomatch
			end
	end;
parameter_value(unicode_binary_wrapped,Parameter,[free,{range,Minor_length,Major_length}]) ->
	if
		is_integer(Minor_length), is_integer(Major_length),
		Minor_length >= 1, Major_length > Minor_length ->
			case parameter_value(unicode_binary_wrapped,Parameter,[free,free]) of
				nomatch -> nomatch;
				Binary_parameter ->
					Size = byte_size(Binary_parameter),
					if
						Size >= Minor_length, Size =< Major_length -> Binary_parameter;
						true -> nomatch
					end
			end;
		true -> a:error(?FUNCTION_NAME(),a003)
	end;
parameter_value(unicode_binary_wrapped,Parameter,[{except,Exception_chars},Length_type]) ->
	case io_lib:char_list(Exception_chars) of
		true ->
			Binary = unicode:characters_to_binary(Parameter),
			Exception = unicode:characters_to_binary(Exception_chars),
			Pattern = fun() ->
				case Length_type of
					free ->
						<<("^(<<\"){1}((?!\">>|[")/utf8,(Exception)/binary,("]).){1,}(\">>)$")/utf8>>;
					{less_equal,Length} ->
						if
							is_integer(Length), Length >= 2 ->
								<<("^(<<\"){1}((?!\">>|[")/utf8,(Exception)/binary,
									("]).){1,")/utf8,(integer_to_binary(Length))/binary,
									("}(\">>)$")/utf8>>;
							true -> a:error(?FUNCTION_NAME(),a003)
						end;
					{size,Length} ->
						if
							is_integer(Length), Length >= 2 ->
								<<("^(<<\"){1}((?!\">>|[")/utf8,(Exception)/binary,
									("]).){")/utf8,(integer_to_binary(Length))/binary,
									("}(\">>)$")/utf8>>;
							true -> a:error(?FUNCTION_NAME(),a003)
						end;
					{range,Minor_length,Major_length} ->
						if
							is_integer(Minor_length),is_integer(Major_length),
							Minor_length >= 1, Major_length > Minor_length ->
								<<("^(<<\"){1}((?!\">>|[")/utf8,(Exception)/binary,
									("]).){")/utf8,(integer_to_binary(Minor_length))/binary,
									(",")/utf8,(integer_to_binary(Major_length))/binary,
									("}(\">>)$")/utf8>>;
							true -> a:error(?FUNCTION_NAME(),a003)
						end
				end
			end,
			case Pattern() of
				{error,Reason} -> {error,Reason};
				Pattern_binary ->
					case re:run(Binary,Pattern_binary) of
						nomatch -> nomatch;
						{match,_} ->
							Size = byte_size(Binary),
							binary:part(Binary,3,Size-6)
					end
			end;
		false -> a:error(?FUNCTION_NAME(),a014)
	end;
%% Range positive integer, regex rule ^([0-9]{1,})\:([0-9]{1,})$
%% Range negative integer, regex rule ^(\-[0-9]{1,}|0)\:(\-[0-9]{1,}|0)$
%% Range integer, regex rule ^(\-?[0-9]{1,})\:(\-?[0-9]{1,})$
parameter_value(Parameter_type,Parameter,_)
	when
		Parameter_type == range_pos_integer;
		Parameter_type == range_neg_integer;
		Parameter_type == range_integer	->
	Pattern = fun() ->
		case Parameter_type of
			range_pos_integer -> "^([0-9]{1,})\:([0-9]{1,})$";
			range_neg_integer -> "^(\-[0-9]{1,}|0)\:(\-[0-9]{1,}|0)$";
			range_integer -> "^(\-?[0-9]{1,})\:(\-?[0-9]{1,})$"
		end
	end,
	case re:split(Parameter,Pattern(),[{return,list}]) of
		[_,Value1_string,Value2_string,_] ->
			Value1 = list_to_integer(Value1_string),
			Value2 = list_to_integer(Value2_string),
			if
				Value1 > Value2 -> Major = Value1, Minor = Value2 ;
				true -> Major = Value2, Minor = Value1
			end,
			{Minor,Major};
		[_] -> nomatch;
		_ -> nomatch
	end;
%% Limited range integer
parameter_value(limited_range_integer,Parameter,[{MinorA,MajorA},{MinorB,MajorB}])
	when
		is_integer(MinorA), is_integer(MajorA),
		is_integer(MinorB), is_integer(MajorB) ->
	case parameter_value(range_integer,Parameter,[]) of
		{error,Reason} -> {error,Reason};
		{A,B} ->
			if
				A > MajorA; A < MinorA -> nomatch;
				B > MajorB; B < MinorB -> nomatch;
				true -> {A,B}
			end;
		nomatch -> nomatch
	end;
%% Range positive float, regex rule ^([0-9]*\.[0-9]*)\:([0-9]*\.[0-9]*)$
%% Range negative float, regex rule ^(\-[0-9]{1,}\.[0-9]{1,}|0)\:(\-[0-9]{1,}\.[0-9]{1,}|0)$
%% Range float, regex rule ^(\-?[0-9]{1,}\.[0-9]{1,})\:(\-?[0-9]{1,}\.[0-9]{1,})$
parameter_value(Parameter_type,Parameter,_)
	when
		Parameter_type == range_pos_float;
		Parameter_type == range_neg_float;
		Parameter_type == range_float	->
	Pattern = fun() ->
		case Parameter_type of
			range_pos_float -> "^([0-9]*\.[0-9]*)\:([0-9]*\.[0-9]*)$";
			range_neg_float -> "^(\-[0-9]{1,}\.[0-9]{1,}|0\.0)\:(\-[0-9]{1,}\.[0-9]{1,}|0\.0)$";
			range_float -> "^(\-?[0-9]{1,}\.[0-9]{1,})\:(\-?[0-9]{1,}\.[0-9]{1,})$"
		end
	end,
	case re:split(Parameter,Pattern(),[{return,list}]) of
		[_,Value1_string,Value2_string,_] ->
			Value1 = list_to_float(Value1_string),
			Value2 = list_to_float(Value2_string),
			if
				Value1 > Value2 -> Major = Value1, Minor = Value2 ;
				true -> Major = Value2, Minor = Value1
			end,
			{Minor,Major};
		[_] -> nomatch;
		_ -> nomatch
	end;
%% Limited range integer
parameter_value(limited_range_float,Parameter,[{MinorA,MajorA},{MinorB,MajorB}])
	when
		is_float(MinorA), is_float(MajorA),
		is_float(MinorB), is_float(MajorB) ->
	case parameter_value(range_float,Parameter,[]) of
		{error,Reason} -> {error,Reason};
		{A,B} ->
			if
				A > MajorA; A < MinorA -> nomatch;
				B > MajorB; B < MinorB -> nomatch;
				true -> {A,B}
			end;
		nomatch -> nomatch
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