-module(safe3).
-export([start/0, loop/1, start_loop/0, backup_loop/1, create_or_update_backup/1, create_backup_unless_exists/0]).

start_loop() ->
  backup ! {state_request, self()},
  receive
    {current_state, State} -> loop(State)
  end.

loop(State) ->
  timer:sleep(1000),
  NewState = State + 1,
  create_or_update_backup(NewState),
  io:format("State is now ~p~n", [NewState]),
  loop(NewState).

backup_loop(State) ->
  receive
    {new_state, NewState} -> backup_loop(NewState);
    {state_request, Pid} ->
      Pid ! {current_state, State},
      backup_loop(State)
  end.

start() ->
  create_backup_unless_exists(),
  Pid = spawn(?MODULE, start_loop, []),
  register(?MODULE, Pid).

create_backup_unless_exists() ->
  BackupExists = is_pid(whereis(backup)),
  if
    BackupExists -> ok;
    true ->
      BackupPid = spawn(?MODULE, backup_loop, [0]),
      register(backup, BackupPid)
  end.

create_or_update_backup(State) ->
  BackupExists = is_pid(whereis(backup)),
  if
    BackupExists -> 
      backup ! {new_state, State};
    true ->
      BackupPid = spawn(?MODULE, backup_loop, [State]),
      register(backup, BackupPid)
  end.

