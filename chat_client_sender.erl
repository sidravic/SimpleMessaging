-module(chat_client_sender).
-export([start/0, stop/0, add_sender/2, loop/1, get_sender/1, send_message/2]).
-define(SERVER, chat_client_sender).
% -define(SENDER_SERVER, chat_client_sender_2).


start() ->
	erlang:register(?SERVER, spawn(chat_client_sender, loop, [dict:new()])).

stop() ->
	?SERVER ! stop.

add_sender(SenderPid, {Uid, Nickname}) ->
	io:format("Adding Sender ~p ~p ~p ~n", [SenderPid, Uid, Nickname]),
	?SERVER ! {add_sender, SenderPid, {Uid, Nickname}}.

get_sender(SenderPid) ->
	?SERVER ! {get_sender, SenderPid}.

send_message(ToUid, MessageBody) ->
	?SERVER ! {send_msg, ToUid, MessageBody}.

loop(MemberPids) ->
	receive
		{add_sender, SenderPid, {Uid, Nickname}} ->
			case dict:find(SenderPid, MemberPids) of
				{ok, {_Uid, _Nickname}} ->	
					io:format("Pid exists ~n"),				
					loop(MemberPids);
				error ->
					loop(dict:store(SenderPid, {Uid, Nickname}, MemberPids))
			end;
		{send_msg, ToUid, MessageBody, SenderPid} ->
			case get_sender(SenderPid, MemberPids) of
				{found, _Uid, Nickname} ->
					message_router:send_message(ToUid, MessageBody, Nickname),
					loop(MemberPids);
				not_found ->
					not_found,
					loop(MemberPids)
			end;
		{remove_sender, SenderPid} ->
			case get_sender(SenderPid, MemberPids) of
				{found, _Uid, _Nickname} ->
					loop(dict:erase(SenderPid, MemberPids));
				not_found ->
					ignored,
					loop(MemberPids)
			end;
		{get_sender, SenderPid} ->
			case get_sender(SenderPid, MemberPids) of
				Any -> 
					io:format("GET SENDER ~p~n", [Any])
			end,
			loop(MemberPids);
		stop ->
			ok
	end.


get_sender(SenderPid, MemberPids) ->
	case dict:find(SenderPid, MemberPids) of 
		{ok, {Uid, Nickname}} ->
			{found, {Uid, Nickname}};
		error ->
			not_found
	end.
