%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Test module for astructures testing
%%%
%%% @end
%%% Created : 03. Май 2018 21:22
%%%-------------------------------------------------------------------
-module(astr_test).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% API
-export([
	test/0
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc

run(init) ->
	astr_mdb:init([node()]),
	run(body);
run(body) ->
	ok.
	
