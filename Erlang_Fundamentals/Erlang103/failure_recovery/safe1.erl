-module(safe1).
-export([start/0, loop/1]).

% server function
loop(State) ->
  timer:sleep(1000),
  NewState = State + 1,
  io:format("State is now ~p~n", [NewState]),
  loop(NewState).

% client function to start the safe1 process
start() ->
  Pid = spawn_link(?MODULE, loop, [0]),
  register(?MODULE, Pid).
