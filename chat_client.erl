-module(chat_client).
-export([start/0, stop/0, loop/1, login/3, logout/1, send_message/3]).
-define(SERVER, chat_client).

start() ->
	message_router:start().	

stop() ->
	message_router:stop().	

login(Uid, Password, Nickname) ->
	io:format("~p My Pid", [self()]),
	Pid = spawn(chat_client, loop, [Nickname]),		
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

send_message(ToUid, FromUid, MessageBody) ->
	message_router:send_message(ToUid, FromUid, MessageBody).


loop(Nickname) ->
	receive
		{print_msg, Messagebody, SenderNickname} ->
			io:format("~p: ~p to ~p~n", [SenderNickname, Messagebody, Nickname]),
			loop(Nickname);		
		stop -> 
			ok
	end.



	