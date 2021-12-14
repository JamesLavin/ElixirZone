-module(add_one).
-export([start/0, request/1, loop/0]).

% client function that starts the add_one process, registers it, and links to it
start() ->
	register(add_one, spawn_link(add_one, loop, [])).

% client function to send request to add_one process and interpret the response
request(Int) ->
	add_one ! {request, self(), Int},
	receive
		{result, Result} -> Result
	after 1000 -> timeout
	end.

% server function invoked by add_one
loop() ->
	receive
		{request, Pid, Msg} ->
			Pid ! {result, Msg + 1}
	end,
	loop().
