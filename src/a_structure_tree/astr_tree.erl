%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Data model handler: astr_tree
%%%
%%% @end
%%% Created : 28. Апр. 2018 17:02
%%%-------------------------------------------------------------------
-module(astr_tree).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Data types
-include("../data_models/types/types_general.hrl").
-include("../data_models/types/types_a_structure_tree.hrl").

%% Data models
-include("../data_models/records/records_a_structure_tree.hrl").

%% API
-export([
	test/0,
	create/1
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Create structure point
-spec create(Datum) -> {aborted,_Reason} | {atomic,_ResultOfFun}
	when
	Datum :: list_of_records() | record().

create(Datum) -> a_mnesia:transaction_create(Datum).
