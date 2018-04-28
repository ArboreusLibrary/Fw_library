%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc The records definition for the structures handler
%%%
%%% @end
%%% Created : 28. Апр. 2018 11:28
%%%-------------------------------------------------------------------
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").


%% ----------------------------
%% @doc Records for defining tree-like structures

-record(astr_tree,{
	point :: astr_point_id(),
	twig :: astr_twig(),
	parent = 0 :: astr_point_id(),
	parent_weight = 0 :: astr_link_weight(),
	container :: astr_container()
}).
-record(astr_link,{
	point_a :: astr_point_id(),
	point_b :: astr_point_id(),
	weight = 0 :: astr_link_weight()
}).