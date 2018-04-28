%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc The additional module for Mnesia data base
%%%
%%% @end
%%% Created : 28. Апр. 2018 15:29
%%%-------------------------------------------------------------------
-module(a_mnesia).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Data types
-include("../data_models/types/types_general.hrl").

%% API
-export([
	test/0,
	create_schema/1,
	create/1,transaction_create/1,dirty_create/1,
	read_by_ids/2,transaction_read_by_ids/2,dirty_read_by_ids/2,
	select_all/1,transaction_select_all/1,dirty_select_all/1
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Dirty read by Ids from the table handler, wrapper for dirty_read_by_ids_handler/3
-spec dirty_read_by_ids(Table,Ids) -> list_of_records()
	when
	Table :: atom(),
	Ids :: list().

dirty_read_by_ids(Table,Ids) ->
	dirty_read_by_ids_handler(Table,Ids,[]).


%% ----------------------------
%% @doc Dirty read by Ids from the table handler
-spec dirty_read_by_ids_handler(Table,Ids,Output) -> list_of_records()
	when
	Table :: atom(),
	Ids :: list(),
	Output :: list_of_records().

dirty_read_by_ids_handler(_,[],Output) -> Output;
dirty_read_by_ids_handler(Table,[Id|Ids],Output) ->
	dirty_read_by_ids_handler(
		Table,Ids,
		lists:append([Output,mnesia:dirty_read(Table,Id)])
	).


%% ----------------------------
%% @doc Transaction reading data by Ids from the table
-spec transaction_read_by_ids(Table,Ids) -> {aborted,_Reason} | {atomic,list_of_records()}
	when
	Table :: atom(),
	Ids :: list().

transaction_read_by_ids(Table,Ids) ->
	mnesia:transaction(fun() -> read_by_ids(Table,Ids) end).


%% ----------------------------
%% @doc Read by Ids from table, wrapper for read_by_ids_handler/3
-spec read_by_ids(Table,Ids) -> list_of_records()
	when
	Table :: atom(),
	Ids :: list().

read_by_ids(Table,Ids) -> read_by_ids_handler(Table,Ids,[]).


%% ----------------------------
%% @doc Read by Ids from table handler
-spec read_by_ids_handler(Table,Ids,Output) -> list_of_records()
	when
	Table :: atom(),
	Ids :: list(),
	Output :: list_of_records().

read_by_ids_handler(_,[],Output) -> Output;
read_by_ids_handler(Table,[Id|Ids],Output) ->
	read_by_ids_handler(
		Table,Ids,
		lists:append([Output,mnesia:read(Table,Id)])
	).


%% ----------------------------
%% @doc Dirty create rows in the table, wrapper for mnesia:dirty_write/1
-spec dirty_create(Datum) -> ok
	when
	Datum :: list_of_records() | record().

dirty_create(Records) when is_list(Records) ->
	lists:foreach(fun(Record) -> mnesia:dirty_write(Record)	end,Records);
dirty_create(Record) -> dirty_create([Record]).


%% ----------------------------
%% @doc Transactional creating row in the table
-spec transaction_create(Datum) -> {aborted,_Reason} | {atomic,_ResultOfFun}
	when
	Datum :: list_of_records() | record().

transaction_create(Datum) ->
	mnesia:transaction(fun() -> create(Datum) end).


%% ----------------------------
%% @doc Create rows in the table, wrapper for mnesia:write/1
-spec create(Datum) -> ok
	when
	Datum :: list_of_records() | record().

create(Records) when is_list(Records) ->
	lists:foreach(fun(Record) -> mnesia:write(Record) end,Records);
create(Record) -> create([Record]).


%% ----------------------------
%% @doc Dirty select all rows from table
-spec dirty_select_all(Table) -> [_Datum] | {aborted,_Reason}
	when
	Table :: atom().

dirty_select_all(Table) ->
	mnesia:dirty_select(Table,[{'_',[],['$_']}]).


%% ----------------------------
%% @doc Select all from the table transaction
-spec transaction_select_all(Table) -> {aborted,_Reason} | {atomic,list_of_records()}
	when
	Table :: atom().

transaction_select_all(Table) ->
	mnesia:transaction(fun() -> select_all(Table) end).


%% ----------------------------
%% @doc Select all rows from te table for transaction usage
-spec select_all(Table) -> [_Datum] | {aborted,_Reason}
	when
	Table :: atom().

select_all(Table) ->
	mnesia:select(Table,[{'_',[],['$_']}]).


%% ----------------------------
%% @doc Create schema on the defined nodes
-spec create_schema(Nodes) -> ok | error
	when
	Nodes :: [node()].

create_schema(Nodes) ->
	create_schema_handler(mnesia_check,Nodes).


%% ----------------------------
%% @doc Create schema on the defined nodes
-spec create_schema_handler(Action,Nodes) -> ok | error
	when
	Action :: mnesia_check | create | rebuild,
	Nodes :: [node()].

create_schema_handler(mnesia_check,Nodes) ->
	case mnesia:system_info(is_running) of
		yes ->
			mnesia:stop(),
			create_schema_handler(create,Nodes);
		no ->
			create_schema_handler(create,Nodes)
	end;
create_schema_handler(create,Nodes) ->
	ok = create_schema_handler(rebuild,Nodes),
	ok = mnesia:start();
create_schema_handler(rebuild,Nodes) ->
	case mnesia:create_schema(Nodes) of
		{error,{_,{already_exists,_}}} ->
			mnesia:delete_schema(Nodes),
			mnesia:create_schema(Nodes);
		ok -> ok;
		_ -> error
	end.