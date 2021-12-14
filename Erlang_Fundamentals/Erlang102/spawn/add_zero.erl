-module(add_zero).
-export([start/0, request/1, loop/0]).

% client function that starts the add_zero process, registers it, and links to it
start() ->
	register(add_zero, spawn(add_zero, loop, [])).

% client function to send request to add_zero process and interpret the response
request(Int) ->
	add_zero ! {request, self(), Int},
	receive
		{result, Result} -> Result
	after 1000 -> timeout
	end.

% server function invoked by add_zero
loop() ->
	receive
		{request, Pid, Msg} ->
			Pid ! {result, Msg + 0}
	end,
	loop().
