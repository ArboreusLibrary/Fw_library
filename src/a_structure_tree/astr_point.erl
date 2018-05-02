%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Data model handler: astr_points
%%%
%%% @end
%%% Created : 02. Май 2018 15:15
%%%-------------------------------------------------------------------
-module(astr_point).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Data types
-include("../data_models/types/types_general.hrl").
-include("../data_models/types/types_a_structure_tree.hrl").

%% Data models
-include("../data_models/records/records_a_structure_tree.hrl").

%% Constants
-define(ALIAS_TABLE_NAME,?MODULE).
-define(POINTS_TABLE_NAME,astr_point).
-define(MODEL_NAME,?MODULE).

%% API
-export([
	test/0,
	create/1,
	read/1
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Read point from table
-spec read(Astr_point_id) ->
	{norow,Astr_point_id} | {ok,Astr_point} | {error,Astr_point_id}
	when
	Astr_point_id :: astr_point_id(),
	Astr_point :: astr_point().

read(Astr_point_id) when is_binary(Astr_point_id) ->
	case mnesia:dirty_read(?MODEL_NAME,Astr_point_id) of
		[] -> {norow,Astr_point_id};
		[Astr_point] -> {ok,Astr_point};
		_ -> {error,Astr_point_id}
	end.


%% ----------------------------
%% @doc Create new point
-spec create(Record) -> {ok,Astr_point_id} | {error,_Reply}
	when
	Record :: astr_point(),
	Astr_point_id :: astr_point_id().

create(Record) when is_record(Record,astr_point) ->
	case create_new_id() of
		{ok,Astr_point_id} ->
			case mnesia:transaction(fun() ->
				mnesia:write(Record#astr_point{id = Astr_point_id})
			end) of
				{atomic,_} -> {ok,Astr_point_id};
				Reply ->
					ok = mnesia:dirty_delete(?MODEL_NAME,Astr_point_id),
					{error,Reply}
			end;
		Reply -> Reply
	end.


%% ----------------------------
%% @doc Create new id for point
-spec create_new_id() -> {ok,astr_point_id()} | {error,_Reason}.

create_new_id() ->
	New_id = a_sequence:random([numeric,alpha_lower,alpha_upper],12),
	case mnesia:transaction(fun() ->
		case mnesia:read(?MODEL_NAME,New_id) of
			[] -> mnesia:write(#astr_point{id = New_id});
			[_Astr_point] -> mnesia:abort(existed);
			Reply -> Reply
		end
	end) of
		{atomic,_} -> {ok,New_id};
		{aborted,{existed,_}} -> create_new_id();
		Reply -> {error,Reply}
	end.