%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Arboreus self-balanced tree based data structures handler
%%%
%%% @end
%%% Created : 06/23/2018 at 13:33
%%%-------------------------------------------------------------------
-module(a_structure_gb).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Constants

%% Data types

%% Data models

%% API
-export([
	test/0
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() ->
	Time_start = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_structure_gb) testing started at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_start), Time_start]
	),
	Time_stop = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_structure_gb) testing finished at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_stop), Time_stop]
	),
	io:format("Test time is: ~p~n", [Time_stop - Time_start]),
	ok.


verify(Structure,Model) ->
	[].