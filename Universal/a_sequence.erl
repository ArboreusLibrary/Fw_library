%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV (http://alexandr.kirilov.me)
%%% @copyright (C) 2015, Arboreus, (http://arboreus.systems)
%%% @doc
%%%
%%% @end
%%% Created : 30. Jul 2015 1:27
%%%-------------------------------------------------------------------
-module(a_sequence).
-author("Alexandr KIRILOV (http://alexandr.kirilov.me)").
-vsn("0.0.2.96").

%% Module API
-export([dictionaries/0]).
-export([make_dictionary/1]).
-export([random/2,random/3]).
-export([md/2]).
-export([unique/1]).

%% Module Include Start
-include("../Handler/a.hrl").
%% Module Include End

%% @spec dictionaries() -> list()
%% @doc Return a list of registered dictionaries
dictionaries() ->
	[
		{numeric,<<"0123456789">>},
		{alpha_lower,<<"abcdefghijklmnopqrstuvwxyz">>},
		{alpha_upper,<<"ABCDEFGHIJKLMNOPQRSTUVWXYZ">>}
	].

%% @spec make_dictionary(Schema) -> binary() | {error,Reason}
%% where
%%      Schema = list()
%% @doc Return a binary within a dictionary
make_dictionary(Schema) when is_list(Schema) == true ->
	make_dictionary(Schema,<<>>);
make_dictionary(_) -> a:error(?FUNCTION_NAME(),a014).

%% @spec make_dictionary(Schema,Dictionary) -> binary() | {error,Reason}
%% where
%%      Schema = list()
%%      Dictionary = binary()
%% @doc Return a binary within a dictionary
make_dictionary([],Dictionary) when is_binary(Dictionary) == true -> Dictionary;
make_dictionary([Head|Tail],Dictionary) when is_binary(Dictionary) == true ->
	Part = proplists:get_value(Head,dictionaries()),
	case Part of
		undefined ->
			a:error(?FUNCTION_NAME(),m001_001);
		_ ->
			make_dictionary(Tail,<<Part/binary,Dictionary/binary>>)
	end;
make_dictionary(_,_) -> a:error(?FUNCTION_NAME(),a000).

%% @spec random(Dictionary_schema,Length) -> binary() | {error,Reason}
%% where
%%      Dictionary_schema = list(),
%%      Length = integer()
%% @doc Return a binary within sequence
random(Dictionary_schema,Length)
	when
		is_list(Dictionary_schema) == true,
		is_integer(Length) == true, Length > 0 ->
	Dictionary = make_dictionary(Dictionary_schema),
	case Dictionary of
		{error,_} ->
			a:error(?FUNCTION_NAME(),m001_002);
		_ ->
			make_random(Dictionary,Length)
	end;
random(_,_) -> a:error(?FUNCTION_NAME(),a000).

%% @spec random(number,Minor,Major) = integer | {error,Reason}
%% where
%%      Minor = integer(),
%%      Major = integer()
%% @doc Return random value from range between Minor and Major
random(number,Minor,Major)
	when
		is_integer(Minor) == true, Minor >= 0,
		is_integer(Major) == true, Major > Minor ->
	random:uniform(Major-Minor)+Minor;
random(number,_,_) -> a:error(?FUNCTION_NAME(),m001_003);
random(_,_,_) -> a:error(?FUNCTION_NAME(),a000).

%% @spec make_random(Dictionary,Length) -> binary()
%% where
%%      Dictionary_schema = list(),
%%      Length = integer()
%% @doc Return a binary within sequense by the Dictionary
make_random(Dictionary,Length) -> make_random(Dictionary,Length,<<>>).
make_random(_,0,Sequence) -> Sequence;
make_random(Dictionary,Length,Sequence) ->
	Random_byte = binary:part(Dictionary,{random(number,0,byte_size(Dictionary))-1,1}),
	make_random(Dictionary,Length-1,<<Random_byte/binary,Sequence/binary>>).

%% @spec md(Object,Type) -> binary() | {error,Reason}
%% where
%%      Object = object()
%%      Type = md4 | md5
%% @doc Return a binary within a hash from object
md(Object,Type)
	when
	Type == md4; Type == md5 ->
	<<  A1:4, A2:4, A3:4, A4:4, A5:4, A6:4, A7:4, A8:4,
	A9:4, A10:4,A11:4,A12:4,A13:4,A14:4,A15:4,A16:4,
	A17:4,A18:4,A19:4,A20:4,A21:4,A22:4,A23:4,A24:4,
	A25:4,A26:4,A27:4,A28:4,A29:4,A30:4,A31:4,A32:4
	>> = crypto:hash(Type,Object),
	<<  (md_hex(A1)),  (md_hex(A2)),  (md_hex(A3)),  (md_hex(A4)),
	(md_hex(A5)),  (md_hex(A6)),  (md_hex(A7)),  (md_hex(A8)),
	(md_hex(A9)),  (md_hex(A10)), (md_hex(A11)), (md_hex(A12)),
	(md_hex(A13)), (md_hex(A14)), (md_hex(A15)), (md_hex(A16)),
	(md_hex(A17)), (md_hex(A18)), (md_hex(A19)), (md_hex(A20)),
	(md_hex(A21)), (md_hex(A22)), (md_hex(A23)), (md_hex(A24)),
	(md_hex(A25)), (md_hex(A26)), (md_hex(A27)), (md_hex(A28)),
	(md_hex(A29)), (md_hex(A30)), (md_hex(A31)), (md_hex(A32)) >>;
md(_,_) -> a:error(?FUNCTION_NAME(),a000).

md_hex(X) ->
	element(X+1,{$0,$1,$2,$3,$4,$5,$6,$7,$8,$9,$a,$b,$c,$d,$e,$f}).

%% @spec unique(Source) -> binary()
%% where
%%      Source = time | Object()
%% @doc Return a binary within random unique sequence
unique(time) ->
	Time = a:bin(a_time:timestamp()),
	Random_value = make_random(make_dictionary([alpha_lower]),64),
	md(<<Time/binary,Random_value/binary>>,md4);
unique(Object) ->
	Object_sequence = md(Object,md5),
	Random_value = make_random(make_dictionary([alpha_lower,numeric]),64),
	md(<<Object_sequence/binary,Random_value/binary>>,md4).