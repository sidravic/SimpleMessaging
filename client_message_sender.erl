-module(client_message_sender).
-export([start/0, stop/0]).
-define(?SERVER, client_message_sender).


start() ->
	erlang:register(?SERVER, spawn(client_message_sender, loop, []))