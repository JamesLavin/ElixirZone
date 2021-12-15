-module(safe2).
-export([start/0, loop/1, start_loop/0, backup_loop/1]).

start_backup_unless_exists() ->
  BackupExists = is_pid(whereis(backup)),
  if
    BackupExists -> ok;
    true ->
      BackupPid = spawn(?MODULE, backup_loop, [0]),
      register(backup, BackupPid)
  end.

start_loop() ->
  backup ! {state_request, self()},
  receive
    {current_state, State} -> loop(State)
  end.

% server function
loop(State) ->
  timer:sleep(1000),
  NewState = State + 1,
  backup ! {new_state, NewState},
  io:format("State is now ~p~n", [NewState]),
  loop(NewState).

backup_loop(State) ->
  receive
    {new_state, NewState} -> backup_loop(NewState);
    {state_request, Pid} ->
      Pid ! {current_state, State},
      backup_loop(State)
  end.

% client function to start the safe process
start() ->
  start_backup_unless_exists(),
  Pid = spawn(?MODULE, start_loop, []),
  register(?MODULE, Pid).

