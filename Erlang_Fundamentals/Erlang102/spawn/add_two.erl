-module(add_two).
-export([start/0, request/1, loop/0]).

% client function that starts the add_two process, registers it, and links to it
start() ->
	process_flag(trap_exit, true),
	AddTwoPid = spawn_link(add_two, loop, []),
	register(add_two, AddTwoPid),
	{ok, AddTwoPid}.

% client function to send request to add_two process and interpret the response
request(Int) ->
	add_two ! {request, self(), Int},
	receive
		{result, Result} -> Result;
		{'EXIT', _Pid, Reason} -> {error, Reason}
	after 1000 -> timeout
	end.

% server function invoked by add_two
loop() ->
	receive
		{request, Pid, Msg} ->
			Pid ! {result, Msg + 2}
	end,
	loop().
