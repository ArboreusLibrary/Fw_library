%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Add-on for standard code module
%%%
%%% @end
%%% Created : 08. Янв. 2018 20:07
%%%-------------------------------------------------------------------
-module(a_code).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% System include
-include("../data_models/types_general.hrl").

%% API
-export([
	is_module/1,
	is_path/1
]).


%% ----------------------------
%% @doc Check the module for presence in the Erlang environment.
-spec is_module(Module) -> {Module,Path} | false
	when
	Module :: module(),
	Path :: unix_path_string().

is_module(Module) ->
	lists:keyfind(Module,1,code:all_loaded()).


%% ----------------------------
%% @doc Check the path for presence in the Erlang environment.
-spec is_path(Path::unix_path_string()) -> boolean().

is_path(Path) ->
	lists:member(Path,code:get_path()).