%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2016, http://arboreus.system
%%% @doc
%%%
%%% @end
%%% Created : 12. Февр. 2016 18:07
%%%-------------------------------------------------------------------
-module(a_output_xml).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% System includes
-include_lib("stdlib/include/qlc.hrl").

%% Module Include Start
-include("../Configuration/configuration.conf.hrl").

%% API
-export([
	make/2
]).


%% ----------------------------
%% @doc Return a list prepared for Yaws appmod within
%% XML formated information from Mnesia DB
-spec make(Datum,Data_module) -> list()
	when
		Datum :: list() | tuple(),
		Data_module :: atom().

make({atomic,[]},_) ->
	[
		a_http_headers:cache(no),
		?APPLICATION_HEADER_OK
	];
make({atomic,Db_responce},Data_module)
	when
		is_list(Db_responce),
		is_atom(Data_module) ->
	[
		a_http_headers:csv(no_cache,?XML_OUTPUT_FILENAME),
		?APPLICATION_HEADER_OK,
		{'ehtml',[
			unicode:characters_to_list(<<
				("<?xml version=\"1.0\" encoding=\"UTF-8\"?>")/utf8,
				("<root>")/utf8,
				(xml(Data_module,{atomic,Db_responce},<<>>))/binary,
				("</root>")/utf8
			>>)
		]}
	];
make([{count,Count},{atomic,Db_responce}],Data_module)
	when
		is_integer(Count),
		is_list(Db_responce),
		is_atom(Data_module) ->
	[
		a_http_headers:csv(no_cache,?XML_OUTPUT_FILENAME),
		?APPLICATION_HEADER_OK,
		{'ehtml',[
			unicode:characters_to_list(<<
				("<?xml version=\"1.0\" encoding=\"UTF-8\"?>")/utf8,
				("<root>")/utf8,
				("<count>")/utf8,(integer_to_binary(Count))/binary,("</count>")/utf8,
				(xml(Data_module,{atomic,Db_responce},<<>>))/binary,
				("</root>")/utf8
			>>)
		]}
	];
make(_,_) ->
	[
		a_http_headers:cache(no),
		?APPLICATION_HEADER_ERROR("wrong_output_xml_make_parameters")
	].

%% ----------------------------
%% @doc Return binary data within XML element content from proplist
-spec from_proplist(Name,Proplist,Output) -> byte()
	when
		Name :: atom(),
		Proplist :: proplists:proplist(),
		Output :: byte().

from_proplist(Name,[],Output) ->
	Tag_name = atom_to_binary(Name,utf8),
	<<("<")/utf8,Tag_name/binary,(">")/utf8,Output/binary,
		("</")/utf8,Tag_name/binary,(">")/utf8>>;
from_proplist(Name,[{Key_in,Value_in}|List],Output) ->
	Tag_name = atom_to_binary(Key_in,utf8),
	from_proplist(Name,List,<<
		Output/binary,("<")/utf8,Tag_name/binary,(">")/utf8,
		(a_output:value(Value_in))/binary,
		("</")/utf8,Tag_name/binary,(">")/utf8
	>>).


%% ----------------------------
%% @doc Return binary data within XML element content from record
-spec from_record(Data_module,Record_source) -> byte()
	when
		Data_module :: atom(),
		Record_source::tuple().

from_record(Data_module,Record_source) ->
	Proplist = a_proplists:from_record(Data_module,Record_source),
	from_proplist(element(1,Record_source),Proplist,<<>>).


%% ----------------------------
%% @doc Return binary within XML
-spec xml(Data_module,Records,Output) -> byte()
	when
		Data_module :: atom(),
		Records :: list() | tuple(),
		Output :: byte().

xml(Data_module,{_,Records},Output) ->
	xml(Data_module,Records,Output);
xml(_,[],Output) -> Output;
xml(Data_module,[Record|Bulk],Output) ->
	xml(Data_module,Bulk,<<Output/binary,(from_record(Data_module,Record))/binary>>).