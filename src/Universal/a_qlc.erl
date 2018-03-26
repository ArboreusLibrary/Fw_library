%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2016, http://arboreus.system
%%% @doc
%%%
%%% @end
%%% Created : 11. Февр. 2016 17:19
%%%-------------------------------------------------------------------
-module(a_qlc).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").
-vsn("0.0.2.209").

%% API
-export([
	select_ordered/2,
	select_paginated/4
]).

%% Module Include Start
-include("../Handler/a.hrl").
%% Module Include End

%% System includes
-include_lib("stdlib/include/qlc.hrl").


%%-----------------------------------
%% @doc Make request to Mnesia DB within ordering by defined field
-spec select_ordered(Query,Order) -> {atomic,_Result} | {aborted,_Error_notice} | {error,_Unswear}
	when
		Query :: qlc:query_handle(),
		Order :: fun().

select_ordered(Query,Order) ->
	try
		Transaction = fun() -> qlc:eval(qlc:sort(Query,[{order,Order}])) end,
		mnesia:transaction(Transaction)
	catch _:_ -> a:error(?NAME_FUNCTION(),m005_001) end.


%%-----------------------------------
%% @doc Make request to Mnesia DB within ordering by defined field
%% and paginating by values From and To
-spec select_paginated(Query,Order,From,To) -> {atomic,_Result} | {aborted,_Error_notice} | {error,_Unswear}
	when
		Query :: qlc:query_handle(),
		Order :: fun(),
		From :: integer(),
		To :: integer().

select_paginated(Query,Order,From,To)
	when
		is_integer(From), is_integer(To),
		From >= 1, From < To ->
	try
		Transaction = fun() ->
			Cursor = qlc:cursor(qlc:sort(Query,[{order,Order}])),
			if
				From == 1 ->
					Result = qlc:next_answers(Cursor,To),
					qlc:delete_cursor(Cursor),
					Result;
				true ->
					qlc:next_answers(Cursor,From-1),
					Result = qlc:next_answers(Cursor,To-From+1),
					qlc:delete_cursor(Cursor),
					Result
			end
		end,
		mnesia:transaction(Transaction)
	catch _:_ -> a:error(?NAME_FUNCTION(),m005_001) end;
select_paginated(_,_,_,_) -> a:error(?NAME_FUNCTION(),a009).