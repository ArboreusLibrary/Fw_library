%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2018, http://arboreus.system
%%% @doc Arboreus users: a_user data model handler
%%%
%%% @end
%%% Created : 06/03/2018 at 13:26
%%%-------------------------------------------------------------------
-module(a_user).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Constants
-define(TABLE,?MODULE).
-define(SALT,<<("salt_password")/utf8>>).

%% Data types
-include("../data_models/types/types_general.hrl").
-include("../data_models/types/types_time.hrl").
-include("../data_models/types/types_a_users.hrl").

%% Data models
-include("../data_models/records/records_a_users.hrl").

%% API
-export([
	test/0,
	create/1,
	read/1,
	update/1,
	delete/1,
	verify_password/2
]).


%% ----------------------------
%% @doc Module test function
-spec test() -> ok.

test() ->
	Time_start = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_user) testing started at:~n~p (~p)~n",
		[unicode:characters_to_list(a_time:current(rfc850)),Time_start]
	),
	Password_string = "test",
	Password_hash = generate_password_hash(Password_string),
	{ok,A_user_id} = create(#a_user{password = Password_hash}),
	io:format("DONE! User created: ~p~n",[A_user_id]),
	Fake_user_id = "fake_id",
	{norow,Fake_user_id} = read(Fake_user_id),
	{ok,A_user = #a_user{id = A_user_id,password = Password_hash}} = read(A_user_id),
	io:format("DONE! User data read: ~p~n",[A_user]),
	New_password_string = "new_test",
	New_password = generate_password_hash(New_password_string),
	{ok,A_user_id} = update(A_user#a_user{password = New_password}),
	{ok,#a_user{id = A_user_id,password = New_password}} = read(A_user_id),
	io:format("DONE! User data update: ~p~n",[A_user_id]),
	{ok,A_user_id} = verify_password(A_user_id,New_password_string),
	{wrong_password,A_user_id} = verify_password(A_user_id,Password_string),
	{norow,_} = verify_password(1,1),
	io:format("DONE! Password verification finished for user: ~p~n",[A_user_id]),
	{ok,A_user_id} = delete(A_user_id),
	{norow,A_user_id} = read(A_user_id),
	io:format("DONE! User deleting finished: ~p~n",[A_user_id]),
	Time_stop = a_time:current(timestamp),
	io:format("*** -------------------~n"),
	io:format(
		"Module (a_user) testing finished at:~n~p (~p)~n",
		[unicode:characters_to_list(a_time:current(rfc850)),Time_stop]
	),
	io:format("Test time is: ~p~n",[Time_stop - Time_start]),
	ok.

%% ----------------------------
%% @doc Verify password for user
-spec verify_password(User,Password) ->
	{ok,a_user_id()} | {norow,User} | {wrong_password,a_user_id()} | {error,User}
	when
	User :: a_user_id(),
	Password :: utf_text_string().

verify_password(User,Password) ->
	case read(User) of
		{ok,A_user} ->
			Password_hash = generate_password_hash(Password),
			if
				Password_hash == A_user#a_user.password -> {ok,A_user#a_user.id};
				true -> {wrong_password,A_user#a_user.id}
			end;
		{norow,User} -> {norow,User};
		_ -> {error,User}
	end.


%% ----------------------------
%% @doc Delete a_user record from db
-spec delete(Key) -> {ok,Key} | {norow,Key} | {error,_Reason}
	when
	Key :: a_user_id().

delete(Key) -> a_mnesia:transaction_delete(?TABLE,Key).


%% ----------------------------
%% @doc Update a_user record
-spec update(Record) -> {ok,a_user_id()} | {norow,a_user_id()} | {error,_Reason}
	when
	Record :: a_user().

update(Record) when is_record(Record,a_user) ->
	case mnesia:transaction(fun() ->
		case mnesia:read(?TABLE,Record#a_user.id) of
			[] -> mnesia:abort(norow);
			[_A_user] -> mnesia:write(Record);
			Read_result -> mnesia:abort({error,Read_result})
		end
	end) of
		{atomic,_} -> {ok,Record#a_user.id};
		{aborted,norow} -> {norow,Record#a_user.id};
		Transaction_result -> {error,Transaction_result}
	end.


%% ----------------------------
%% @doc Read a_user record from db
-spec read(A_user_id) -> {norow,A_user_id} | {ok,a_user()}
	when
	A_user_id :: a_user_id().

read(A_user_id) -> a_mnesia:dirty_read(?TABLE,A_user_id).


%% ----------------------------
%% @doc Create new a_user
-spec create(Record) -> {ok,a_user_id()} | {error,_Reason}
	when
	Record :: a_user().

create(Record) when is_record(Record,a_user) ->
	case a_mnesia:transaction_generate_unique(
		Record,[numeric,alpha_lower,alpha_upper],8
	) of
		{atomic,{ok,Id}} -> {ok,Id};
		Reply -> {error,Reply}
	end.


%% ----------------------------
%% @doc Generate password hash with salt inclusion
-spec generate_password_hash(Password) -> md5_binary()
	when
	Password :: utf_text_string() | utf_text_binary().

generate_password_hash(Password) when is_list(Password) ->
	a_sequence:make_password_hash(Password,?SALT,md5).