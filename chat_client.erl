-module(chat_client).
-export([start/0, stop/0, print_msg/1, login/3, logout/1]).
-define(SERVER, chat_client).

start() ->
	message_router:start().

stop() ->
	message_router:stop().

login(Uid, Password, Nickname) ->
	Pid = spawn(chat_client, print_msg, [Nickname]),
	erlang:register(?SERVER, Pid),	
	case message_router:login(Uid, Password, Nickname, Pid) of
		{ok, logged_in} ->
			{ok, logged_in};
		{error, invalid_uid_or_pwd} ->
			{error, invalid}
	end.

logout(Uid) ->
	case message_router:logout(Uid) of
		{ok, logged_out} ->
			{ok, logged_out};		
		ignored ->
			ignored;
		_Someshit ->
			io:format("Some Shit ~p", _Someshit)
	end.


print_msg(Nickname) ->
	receive
		{printmsg, Messagebody, Sender} ->
			io:format("~p says: ~p to ~p~n", [Messagebody, Sender, Nickname]),
			print_msg(Nickname);
		stop -> 
			ok
	end.



	