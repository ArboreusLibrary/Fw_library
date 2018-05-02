%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Data model handler: astr_alias
%%%
%%% @end
%%% Created : 02. Май 2018 11:44
%%%-------------------------------------------------------------------
-module(astr_alias).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Data types
-include("../data_models/types/types_general.hrl").
-include("../data_models/types/types_a_structure_tree.hrl").

%% Data models
-include("../data_models/records/records_a_structure_tree.hrl").

%% Constants
-define(ALIAS_TABLE_NAME,?MODULE).
-define(POINTS_TABLE_NAME,astr_point).

%% API
-export([
	test/0,
	create/1,
	read/1,
	update/2,update_point/2,update_description/2,
	delete/1
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Delete alias from DB
-spec delete(Alias) ->
	{ok,Astr_alias_id} | {norow,Astr_alias_id} | {error,Astr_alias_id}
	when
	Alias :: astr_alias() | astr_alias_id().

delete(Record) when is_record(Record,astr_alias) ->
	delete(Record#astr_alias.alias);
delete(Astr_alias_id) ->
	case mnesia:transaction(fun() ->
		case mnesia:read(Astr_alias_id) of
			[] -> mnesia:abort({norow,Astr_alias_id});
			[Astr_alias] -> mnesia:delete_object(Astr_alias)
		end
	end) of
		{atomic,_ResultOfFun} -> {ok,Astr_alias_id};
		{aborted,{norow,Astr_alias_id}} -> {norow,Astr_alias_id};
		_ -> {error,Astr_alias_id}
	end.


%% ----------------------------
%% @doc Update description for alias
-spec update_description(Description,Record) ->
	{ok,astr_alias_id()} | {nopoint,astr_point_id()} | {error,astr_alias_id()}
	when
	Description :: astr_alias_description(),
	Record :: astr_alias().

update_description(Description,Record) -> update([{description,Description}],Record).


%% ----------------------------
%% @doc Update point for alias
-spec update_point(Point,Record) ->
	{ok,astr_alias_id()} | {nopoint,astr_point_id()} | {error,astr_alias_id()}
	when
	Point :: astr_point_id(),
	Record :: astr_alias().
	
update_point(Point,Record) -> update([{point,Point}],Record).


%% ----------------------------
%% @doc Update alias in the DB
-spec update(Values,Record) ->
	{ok,astr_alias_id()} | {nopoint,astr_point_id()} | {error,astr_alias_id()}
	when
	Values :: proplists:proplist(),
	Record :: astr_alias().

update(Values,Record) when is_record(Record,astr_alias) ->
	case mnesia:transaction(fun() ->
		mnesia:write(Record#astr_alias{
			point = case proplists:get_value(point,Values) of
				undefined -> Record#astr_alias.point;
				Point ->
					case mnesia:read(?POINTS_TABLE_NAME,Point) of
						[] -> mnesia:abort({nopoint,Point});
						[_Astr_points] -> Point
					end
			end,
			description = case proplists:get_value(description,Values) of
				undefined -> Record#astr_alias.description;
				Description -> Description
			end
		})
	end) of
		{atomic,_ResultOfFun} -> {ok,Record#astr_alias.alias};
		{aborted,{nopoint,Point}} -> {nopoint,Point};
		_ -> {error,Record#astr_alias.alias}
	end.


%% ----------------------------
%% @doc Read alias by ID
-spec read(Astr_alias_id) ->
	{norow,Astr_alias_id} | {ok,Astr_alias} | {error,Astr_alias_id}
	when
	Astr_alias_id :: astr_alias_id(),
	Astr_alias :: astr_alias().

read(Astr_alias_id) ->
	case mnesia:dirty_read(?ALIAS_TABLE_NAME,Astr_alias_id) of
		[] -> {norow,Astr_alias_id};
		[Astr_alias] -> {ok,Astr_alias};
		_ -> {error,Astr_alias_id}
	end.


%% ----------------------------
%% @doc Create new alias for structure point
-spec create(Record) ->
	{ok,_Astr_alias} | {existed,Astr_alias} | {aborted,_Reason}
	when
	Record :: astr_alias(),
	Astr_alias :: astr_alias().

create(Record) when is_record(Record,astr_alias) ->
	case mnesia:transaction(fun() ->
		case mnesia:read(?POINTS_TABLE_NAME,Record#astr_alias.point) of
			[_Astr_tree_point] ->
				case mnesia:read(?ALIAS_TABLE_NAME,Record#astr_alias.alias) of
					[] ->
						case mnesia:write(Record) of
							ok -> ok;
							Result_of_write -> mnesia:abort(Result_of_write)
						end;
					[Astr_alias] -> mnesia:abort({existed,Astr_alias#astr_alias.alias});
					Result_of_read -> mnesia:abort(Result_of_read)
				end;
			[] -> mnesia:abort({nopoint,Record#astr_alias.point});
			Result_of_point_read -> mnesia:abort(Result_of_point_read)
		end
	end) of
		{atomic,_ResultOfFun} -> {ok,Record#astr_alias.alias};
		{aborted,{existed,Astr_alias}} -> {existed,Astr_alias};
		Result_of_transaction -> Result_of_transaction
	end.