%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Arboreus matrix handler
%%%
%%% @end
%%% Created : 06/19/2018 at 21:43
%%%-------------------------------------------------------------------
-module(a_matrix).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Constants

%% Data types
-include("../data_models/types/types_general.hrl").

%% Data models

%% API
-export([
	test/0,
	check_equality/1,
	lesser/1,
	greater/1,
	arrange/1
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() ->
	Time_start = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_matrix) testing started at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_start), Time_start]
	),
	Matrix1 = [1,1,1,0],
	Matrix2 = [2,4,1,1],
	Matrix3 = [3,5,6,1],
	Matrices = [Matrix1,Matrix2,Matrix3],
	Not_equal_matrices1 = lists:append(Matrices,[1]),
	Not_equal_matrices2 = lists:append(Matrices,[1,1,2,false]),
	true = check_equality(Matrices),
	false = check_equality(Not_equal_matrices1),
	false = check_equality(Not_equal_matrices2),
	io:format("DONE! Fun check_equality/1 test passed~n"),
	Time_stop = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_matrix) testing finished at:~n~p (~p)~n",
		[a_time:from_timestamp(rfc850, Time_stop), Time_stop]
	),
	io:format("Test time is: ~p~n", [Time_stop - Time_start]),
	ok.


%% ----------------------------
%% @doc

lesser(Matrices) when is_list(Matrices) ->
	[].


%% ----------------------------
%% @doc

greater(Matrices) when is_list(Matrices) ->
	[].


%% ----------------------------
%% @doc

arrange(Matrices) when is_list(Matrices) ->
	[].


%% ----------------------------
%% @doc Check matrices for equality to each other
-spec check_equality(Matrices) -> boolean()
	when
	Matrices :: list_of_lists().

check_equality(Matrices) ->
	check_equality_handler(start,Matrices,return_boolean).


%% ----------------------------
%% @doc Check matrices for equality to each other
-spec check_equality_handler(Kind,Matrices,Return_mode) -> boolean() | {ok,Structure}
	when
	Kind :: start | {structure,Structure},
	Structure :: list_of_functions(),
	Matrices :: list_of_lists(),
	Return_mode :: boolean() | structure.

check_equality_handler(_,[],return_boolean) -> true;
check_equality_handler({structure,Structure},[],return_structure) -> {ok,Structure};
check_equality_handler(start,[First_matrix|Matrices],Return_mode) when is_list(First_matrix) ->
	check_equality_handler(
		{structure,a_list:get_structure(verificator,First_matrix,[])},
		Matrices,Return_mode
	);
check_equality_handler({structure,Structure},[Matrix|Matrices],Return_mode) when is_list(Matrix) ->
	case a_list:structure_equality(Matrix,Structure) of
		true -> check_equality_handler({structure,Structure},Matrices,Return_mode);
		_ -> false
	end;
check_equality_handler(_,_,_) -> false.