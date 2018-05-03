%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Data model handler: astr_link
%%%
%%% @end
%%% Created : 02. Май 2018 13:04
%%%-------------------------------------------------------------------
-module(astr_link).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Data types
-include("../data_models/types/types_general.hrl").
-include("../data_models/types/types_a_structure_tree.hrl").

%% Data models
-include("../data_models/records/records_a_structure_tree.hrl").

%% Constants
-include("astr_constants.hrl").
-define(MODEL_NAME,?NAME_LINK).

%% API
-export([
	test/0,
	create/1,
	read/1,
	update_strength/2,
	select/3,dirty_select/3
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() ->
	Link1 = #astr_link{point_a = 1,point_b = 2},
	Link2 = #astr_link{point_a = 2,point_b = 3},
	Link3 = #astr_link{point_a = 1,point_b = 3},
	Link4 = #astr_link{point_a = 2,point_b = 4},
	{ok,Id1} = create(Link1),
	{ok,_} = create(Link2),
	{ok,_} = create(Link3),
	{existed,_} = create(Link3),
	{ok,_} = create(Link4),
	io:format("Ok. Creating test users finished ~n"),
	{ok,#astr_link{point_a = 1,point_b = 2}} = read(Id1),
	io:format("Ok. reading test user finished ~n"),
	ok.


%% ----------------------------
%% @doc Transactional select from DB
-spec select(Kind,Properties,Return_mode) ->
	{ok,_Datum} | {norow,Properties} | {aborted,_Reason}
	when
	Kind :: by_point | by_points,
	Properties :: list_of_properties(),
	Return_mode :: return_id | return_ids | return_record | return_records.

select(by_point,[Point],Return_mode) ->
	Match_head = #astr_link{point_a = '$1',point_b = '$2',_ = '_'},
	Guard = {'or',{'==','$1',Point},{'==','$2',Point}},
	Result = '$_',
	case mnesia:select(
		?MODEL_NAME,[{Match_head,[Guard],[Result]}]
	) of
		[] -> {norow,[Point]};
		{aborted,Reason} -> {aborted,Reason};
		Records -> select_return(Return_mode,Records)
	end;
select(by_points,[Point_a,Point_b],Return_mode) ->
	Match_head = #astr_link{point_a = '$1',point_b = '$2',_ = '_'},
	Guard = {'or',
		{'and',{'==','$1',Point_a},{'==','$2',Point_b}},
		{'and',{'==','$1',Point_b},{'==','$2',Point_a}}
	},
	Result = '$_',
	case mnesia:select(
		?MODEL_NAME,[{Match_head,[Guard],[Result]}]
	) of
		[] -> {norow,[Point_a,Point_b]};
		{aborted,Reason} -> {aborted,Reason};
		Records -> select_return(Return_mode,Records)
	end.


%% ----------------------------
%% @doc Dirty select links from DB
-spec dirty_select(Kind,Properties,Return_mode) ->
	{ok,_Datum} | {norow,Properties} | {aborted,_Reason}
	when
	Kind :: by_point | by_points,
	Properties :: list_of_properties(),
	Return_mode :: return_id | return_ids | return_record | return_records.

dirty_select(by_point,[Point],Return_mode) ->
	Match_head = #astr_link{point_a = '$1',point_b = '$2',_ = '_'},
	Guard = {'or',{'==','$1',Point},{'==','$2',Point}},
	Result = '$_',
	case mnesia:dirty_select(
		?MODEL_NAME,[{Match_head,[Guard],[Result]}]
	) of
		[] -> {norow,[Point]};
		{aborted,Reason} -> {aborted,Reason};
		Records -> select_return(Return_mode,Records)
	end;
dirty_select(by_points,[Point_a,Point_b],Return_mode) ->
	Match_head = #astr_link{point_a = '$1',point_b = '$2',_ = '_'},
	Guard = {'or',
		{'and',{'==','$1',Point_a},{'==','$2',Point_b}},
		{'and',{'==','$1',Point_b},{'==','$2',Point_a}}
	},
	Result = '$_',
	case mnesia:dirty_select(
		?MODEL_NAME,[{Match_head,[Guard],[Result]}]
	) of
		[] -> {norow,[Point_a,Point_b]};
		{aborted,Reason} -> {aborted,Reason};
		Records -> select_return(Return_mode,Records)
	end.


%% ----------------------------
%% @doc Select datum from record by defined field
-spec select_return(Datum,Return_mode) -> {ok,_Datum}
	when
	Datum :: astr_link() | list_of_records(),
	Return_mode :: return_id | return_ids | return_record | return_records.

select_return(return_id,[Record]) ->
	{ok,Record#astr_link.id};
select_return(return_ids,Records) ->
	{ok,[Record#astr_link.id || Record <- Records]};
select_return(_,Datum) -> {ok,Datum}.


%% ----------------------------
%% @doc Update the link's strength
-spec update_strength(Strength,Properties) ->
	{ok,Astr_link_id} | {norow,Astr_link_id} | {error,_Reason}
	when
	Strength :: astr_link_strength(),
	Properties :: list_of_properties() | astr_link() | astr_link_id(),
	Astr_link_id :: astr_link_id().

update_strength(Strength,[Point_a,Point_b]) ->
	case dirty_select(by_points,[Point_a,Point_b],return_record) of
		{ok,Astr_link} -> update_strength(Strength,Astr_link#astr_link.id);
		{aborted,Reason} -> {error,Reason};
		Reply -> Reply
	end;
update_strength(Strength,Astr_link) when is_record(Astr_link,astr_link) ->
	update_strength(Strength,Astr_link#astr_link.id);
update_strength(Strength,Astr_link_id) ->
	case read(Astr_link_id) of
		{ok,Astr_link} ->
			case mnesia:transaction(fun() ->
				mnesia:write(Astr_link#astr_link{strength = Strength})
			end) of
				{atomic,_} -> {ok,Astr_link_id};
				Reply -> {error,Reply}
			end;
		Reply -> Reply
	end.


%% ----------------------------
%% @doc Read link by ID
-spec read(Astr_link_id) ->
	{norow,Astr_link_id} | {ok,Astr_link} | {error,Astr_link_id}
	when
	Astr_link_id :: astr_link_id(),
	Astr_link :: astr_link().

read(Astr_link_id) ->
	case mnesia:dirty_read(?MODEL_NAME,Astr_link_id) of
		[] -> {norow,Astr_link_id};
		[Astr_link] -> {ok,Astr_link};
		_ -> {error,Astr_link_id}
	end.
	

%% ----------------------------
%% @doc Create new link between points
-spec create(Record) ->
	{ok,Astr_link_id} | {existed,Astr_link_id} | {error,Record}
	when
	Record :: astr_link(),
	Astr_link_id :: astr_link_id().

create(Record) when is_record(Record,astr_link) ->
	case mnesia:transaction(fun() ->
		Astr_link_id = generate_id(Record#astr_link.point_a,Record#astr_link.point_b),
		case mnesia:read(?NAME_ALIAS,Astr_link_id) of
			[] ->
				mnesia:write(Record#astr_link{id = Astr_link_id}),
				Astr_link_id;
			[Astr_link] ->
				mnesia:abort({existed,Astr_link#astr_link.id})
		end
	end) of
		{atomic,Astr_link_id} -> {ok,Astr_link_id};
		{aborted,{existed,Astr_link_id}} -> {existed,Astr_link_id};
		_ -> {error,Record}
	end.


%% ----------------------------
%% @doc Generate ID for link between points A and B
-spec generate_id(Point_a,Point_b) -> md5_binary()
	when
	Point_a :: astr_point_id(),
	Point_b :: astr_point_id().

generate_id(Point_a,Point_b) ->
	a_sequence:md(lists:sort([Point_a,Point_b]),md5).