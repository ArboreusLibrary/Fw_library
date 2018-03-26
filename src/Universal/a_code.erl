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
-vsn("389").

%% API
-export([
	is_module/1,
	is_path/1
]).



%% ----------------------------
%% Includes

-include("../Handler/types_basic.hrl").


%% ----------------------------
%% @doc Check the module for presence in the Erlang environment.
-spec is_module(Module) -> {Module,Path} | false
	when
		Module :: module(),
		Path :: unix_path().

is_module(Module) ->
	lists:keyfind(Module,1,code:all_loaded()).


%% ----------------------------
%% @doc Check the path for presence in the Erlang environment.
-spec is_path(Path::unix_path()) -> boolean().

is_path(Path) ->
	lists:member(Path,code:get_path()).