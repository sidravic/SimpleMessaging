-module(message_router).
-export([register/2, unregister/1, start/0, stop/0, login/4, get_user_with_password/2, loop/1, logout/1]).
-define(SERVER, message_router).

start() ->
	erlang:register(?SERVER, spawn(message_router, loop, [dict:new()])).

stop() ->
	?SERVER ! shutdown,
	{ok, shutdown}.

register(Uid, Password) ->
	?SERVER ! {register, Uid, Password, self()},
	receive
		{ignored, user_exists} ->
			user_exists;
		{ok, created} ->
			{ok, created}
	end.

unregister(Uid) ->
	?SERVER ! {unregister, Uid, self()},
	receive
		not_found ->
			not_found;
		{ok, deleted} ->
			{ok, deleted};
		{verfiy, OtherStatus} ->
			{verify, OtherStatus}
	end.

login(Uid, Password, Nickname, ClientPid) ->
	?SERVER ! {login, Uid, Password, Nickname, ClientPid, self()},
	receive
		{ok, logged_in} ->
			{ok, logged_in};
		{error, invalid_uid_or_pwd} ->
			{error, invalid_uid_or_pwd}
	end.

logout(Uid) ->	
	?SERVER ! {logout, Uid, self()},
	receive
		{ok, logged_out} ->
			{ok, logged_out};
		ignored ->
			ignored
	end.



loop(Members) ->
	receive
		{register, Uid, Password, From} ->
			RegisterInfo = user_directory:create_user(Uid, Password),
			From ! RegisterInfo,
			loop(Members);
		{unregister, Uid, From} ->			
			UnregisterInfo = user_directory:delete_user(Uid),
			From ! UnregisterInfo,
			loop(Members);
		{login, Uid, Password, Nickname, ClientPid, From} ->
			case get_user_with_password(Uid, Password) of
				{ok, valid} ->										
					From !{ok, logged_in},
					loop(dict:store(Uid, {ClientPid, Nickname}, Members));					
				invalid ->
					From ! {error, invalid_uid_or_pwd},
					loop(Members)
			end;
		{logout, Uid, From} ->
			io:format("Arrived ~n"),
			case dict:find(Uid, Members) of
				{ok, {ClientPid, Nickname}} ->
					ClientPid ! stop,
					From ! {ok, logged_out},
					loop(dict:erase(Uid, Members));
				error ->
					From ! ignored,
					loop(Members);
				Oops -> 
					io:format("Invalid Message ~p~n", [Oops]),
					loop(Members)
			end;			
		shutdown -> 
			ok;
		Oops -> 
			io:format("[Message Router][Unknown Message] ~p~n", [Oops]),
			loop(Members)
	end.



%% Utility Functions

get_user_with_password(Uid, Password) ->
	case user_directory:get_user(Uid) of
		{ok, {users, Uid, Password, _CreatedOnVal}} ->						
				{ok, valid};
		{ok, {users, Uid, _Password, _CreatedOn}} ->
				invalid;			
		_AnythingElse ->
			io:format("Final match ~p~n", [_AnythingElse]),
			invalid
	end.
