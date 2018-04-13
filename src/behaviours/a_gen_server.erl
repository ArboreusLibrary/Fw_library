%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Standard gen_server behaviour extension
%%%
%%% @end
%%% Created : 13. Апр. 2018 20:12
%%%-------------------------------------------------------------------
-module(a_gen_server).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% System include
-include("../data_models/types_general.hrl").

%% API
-export([
	test/0,
	g_call/2,
	g_start_link/3,g_start_link/4
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() -> ok.


%% ----------------------------
%% @doc Apply the gen_server global call
-spec g_call(Process_id,Message) -> Reply
	when
	Process_id :: process_id(),
	Message :: term(),
	Reply :: term().

g_call(Process_id,Message) ->
	gen_server:call({global,Process_id},Message).


%% ----------------------------
%% @doc Apply the gen_server global spawn link
-spec g_start_link(Process_id,Module,Arguments) ->
	{ok,_Pid} | ignore | {error,_Error}
	when
	Process_id :: process_id(),
	Module :: atom(),
	Arguments :: term().

g_start_link(Process_id,Module,Arguments) ->
	gen_server:start_link({global,Process_id},Module,Arguments,[]).


%% ----------------------------
%% @doc Apply the gen_server global spawn link
-spec g_start_link(Process_id,Module,Arguments,Options) ->
	{ok,_Pid} | ignore | {error,_Error}
	when
	Process_id :: process_id(),
	Module :: atom(),
	Arguments :: term(),
	Options :: [_Option].

g_start_link(Process_id,Module,Arguments,Options) ->
	gen_server:start_link({global,Process_id},Module,Arguments,Options).