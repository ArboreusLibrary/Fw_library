%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2016, http://arboreus.system
%%% @doc
%%%
%%% @end
%%% Created : 15. Февр. 2016 13:45
%%%-------------------------------------------------------------------
-module(a_output_dsv).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Module Include Start
-include("../Configuration/configuration.conf.hrl").

%% API
-export([
	make/2
]).


%% ----------------------------
%% @doc Return a list prepared for Yaws appmod within
%% DSV formated information from Mnesia DB
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
		a_http_headers:csv(no_cache,?DSV_OUTPUT_FILENAME),
		?APPLICATION_HEADER_OK,
		{'ehtml',[
			unicode:characters_to_list(dsv(Data_module,{atomic,Db_responce},<<>>))
		]}
	];
make([{count,Count},{atomic,Db_responce}],Data_module)
	when
		is_integer(Count),
		is_list(Db_responce),
		is_atom(Data_module) ->
	Dsv = <<(integer_to_binary(Count))/binary,
		("\n")/utf8,
		(dsv(Data_module,{atomic,Db_responce},<<>>))/binary>>,
	[
		a_http_headers:csv(no_cache,?DSV_OUTPUT_FILENAME),
		?APPLICATION_HEADER_OK,
		{'ehtml',[unicode:characters_to_list(Dsv)]}
	];
make(_,_) ->
	[
		a_http_headers:cache(no),
		?APPLICATION_HEADER_ERROR("wrong_output_dsv_make_parameters")
	].


%% ----------------------------
%% @doc Return binary within datum in CSV fromat from list of records
-spec dsv(Data_module,List_of_records,Output) -> byte()
	when
		Data_module :: atom(),
		List_of_records :: list(),
		Output :: byte().

dsv(Data_module,{_,Records},_) -> dsv(first_line,Records,Data_module);
dsv(first_line,Records,Data_module) ->
	[Record|_] = Records,
	[Table_name|_] = tuple_to_list(Record),
	dsv(Data_module,Records,data_schema(apply(Data_module,rec_info,[Table_name]),<<>>));
dsv(_,[],Output) -> Output;
dsv(Data_module,[Record|Records],Output) ->
	Output_out = <<Output/binary,(row_from_record(Record))/binary>>,
	dsv(Data_module,Records,Output_out).


%% ----------------------------
%% @doc Return binary within row formatted for CSV file
-spec row_from_record(Record::tuple()) -> byte().

row_from_record(Record) ->
	[_|List] = tuple_to_list(Record),
	make_row(List,<<>>).


%% ----------------------------
%% @doc Auxiliary function for row_from_record(Record), return a unicode binary
%% formatted for DSV files
-spec make_row(Row_list,Output) -> byte()
	when
	Row_list :: list(),
	Output :: byte().

make_row([],Output) -> Output;
make_row([Field_value|List_from_record],Output) ->
	Value = a_output:value(Field_value),
	Dsv_out = fun() ->
		case List_from_record of
			[] -> <<Output/binary,Value/binary,"\n">>;
			_ -> <<Output/binary,Value/binary,?DSV_SEPARATOR>>
		end
	end,
	make_row(List_from_record,Dsv_out()).


%% ----------------------------
%% @doc Return binary within datum schema from record
-spec data_schema(Schema,Output) -> Output
	when
		Schema :: list(),
		Output :: byte().

data_schema([],Output) -> Output;
data_schema([Field|Schema],Output) ->
	Field_name = atom_to_binary(Field,utf8),
	Output_out = fun() ->
		case Schema of
			[] -> <<Output/binary,Field_name/binary,"\n">>;
			_ -> <<Output/binary,Field_name/binary,?DSV_SEPARATOR>>
		end
	end,
	data_schema(Schema,Output_out()).