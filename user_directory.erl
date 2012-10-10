-module(user_directory).
-compile(export_all).
-define(SERVER, user_directory).
-include_lib("stdlib/include/qlc.hrl").
-record(users, {uid, password, created_on}).

%% Interfacing functions

start() ->
	erlang:register(?SERVER, spawn(user_directory, loop, [true])).

stop() ->
	?SERVER ! shutdown.

create_user(Uid, Password) ->
	?SERVER ! {create_user, Uid, Password, self()},
	receive
		user_exists ->
			{ignored, user_exists};
		{multiple_records, Users} ->
			{ignored, multiple_users_exist, Users};
		_ ->
			{ok,created}
	end.

get_user(Uid) ->
	?SERVER ! {get_user, Uid, self()},
	receive
		{ok, User} ->
			io:format("User Found: ~n"),
			{ok, User};
		not_found ->
			io:format("user Not found"),
			not_found;			
		AnythingElse ->
			io:format("[WOOPS] ~n"),
			io:format("Get User returned ~p~n", [AnythingElse])
	end.

delete_user(Uid) ->
	?SERVER ! {delete_user, Uid, self()},
	receive
		not_found -> 
			not_found;
		{atomic, ok} ->
			{ok, deleted};
		OtherStatus ->
			{verify, OtherStatus}
	end.


%% Main Run Loop

loop(FirstTime) ->
	if 
		FirstTime == true ->
			user_directory:init_directory(),
			loop(false);
		true ->
			receive
				{create_user, Uid, Password, From} ->
					CreateInfo = util_create_user(Uid, Password),
					From ! CreateInfo,
					loop(isFirstTime);
				{get_user, Uid, From} ->
					UserInfo = util_get_user(Uid),	
					From ! UserInfo,
					loop(isFirstTime);
				{delete_user, Uid, From} ->
					DeleteInfo = user_directory:util_delete_user(Uid),
					From ! DeleteInfo,
					loop(isFirstTime);
				shutdown ->
					io:format("Stopping Mnesia..."),
					mnesia:stop()
			end
	end.


%% Utility functions

util_create_user(Uid, Password) ->	
	case user_directory:util_get_user(Uid) of
		{ok, _User} ->
			user_exists;
		not_found ->
			F = fun() ->
				{_, CreatedOn,_} =  erlang:now(),
				NewUser = #users{uid=Uid, password=Password, created_on=CreatedOn},
				mnesia:write(NewUser)
			end,
			mnesia:transaction(F);
		{multiple_records, Users} ->
			{multiple_records, Users}			
	end.	


util_get_user(Uid) ->
	F = fun() ->
			Query = qlc:q([X || X <- mnesia:table(users),
								X#users.uid =:= Uid]),
			Result = qlc:e(Query),
			io:format("~p~n", [Result]),
			Result
		end,
		{atomic, User} = mnesia:transaction(F),
		io:format("transaction response ~p~n", [User]),
		io:format("User is ~p~n", [User]),				
		if 
			length(User) == 0 ->
				not_found;
			true -> 
				if
					length(User) == 1 ->
						User1 = lists:last(User),
						{ok, User1};
				true ->
					{multiple_records, User}
				end
		end. 															


util_delete_user(Uid) ->	 
	case user_directory:util_get_user(Uid) of
		{ok, User} ->						
			F = fun() ->
					mnesia:delete_object(User)
				end,
			mnesia:transaction(F);			
		not_found ->
			not_found
	end.

 
init_directory() ->
	mnesia:create_schema([node()]),
	mnesia:start(),
	try
		mnesia:table_info(user_directory, type)
	catch
		exit:_ ->
			mnesia:create_table(users, [{attributes, record_info(fields, users)}, {disc_copies, [node()]}])
	end.




