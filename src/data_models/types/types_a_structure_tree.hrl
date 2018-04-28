%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Data types definition for a_structure
%%%
%%% @end
%%% Created : 28. Апр. 2018 11:40
%%%-------------------------------------------------------------------
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").


%% ----------------------------
%% @doc Structure data models definitions

-type astr_tree() :: {
	astr_tree,
	astr_point_id(),
	astr_twig(),
	astr_parent_point(),
	astr_link_weight(),
	astr_container()
}.
-type astr_link() :: {
	astr_link,
	astr_point_id(),
	astr_point_id(),
	astr_link_weight()
}.

-type astr_point_id() :: <<_:32>>.
-type astr_twig() :: any().
-type astr_parent_point() :: astr_point_id().
-type astr_link_weight() :: 0 | integer().
-type astr_container() :: any().