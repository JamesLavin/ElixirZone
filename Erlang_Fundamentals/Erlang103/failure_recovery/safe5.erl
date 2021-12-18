-module(safe5).

-export([start/0, loop/1, start_loop/0, backup_loop/1, create_or_update_backup/2,
         create_backup_unless_exists/1]).

-num_backups(2).

% safe4:module_info(attributes).
% [{vsn,[194967926018047040463426239172309516572]},
%  {num_backups,[1]}]
%

start_loop() ->
  backup1 ! {state_request, self()},
  receive
    {current_state, State} ->
      loop(State)
  end.

loop(State) ->
  timer:sleep(1000),
  NewState = State + 1,
  create_or_update_backup(backup, NewState),
  io:format("State is now ~p~n", [NewState]),
  loop(NewState).

backup_loop({BackupNum, State}) ->
  receive
    {new_state, NewState} ->
      backup_loop({BackupNum, NewState});
    {state_request, Pid} ->
      Pid ! {current_state, State},
      backup_loop({BackupNum, State})
  end.

start() ->
  create_backup_unless_exists(1),
  Pid = spawn(?MODULE, start_loop, []),
  register(?MODULE, Pid).

% create a cache/backup process to hold state for fault-recovery
create_backup_unless_exists(BackupNum, InitialState) ->
  BackupAtom = backup_atom(BackupNum),
  BackupExists = is_pid(whereis(BackupAtom)),
  if BackupExists ->
       ok;
     true ->
       BackupPid = spawn(?MODULE, backup_loop, [{BackupNum, InitialState}]),
       register(BackupAtom, BackupPid)
  end.

create_backup_unless_exists(BackupNum) ->
  create_backup_unless_exists(BackupNum, 0).

create_or_update_backup(BackupAtom, State) ->
  BackupExists = is_pid(whereis(BackupAtom)),
  if BackupExists ->
       BackupAtom ! {new_state, State};
     true ->
       BackupPid = spawn(?MODULE, backup_loop, [{BackupAtom, State}]),
       register(BackupAtom, BackupPid)
  end.

num_backups() ->
  lists:nth(1, element(2, lists:keyfind(num_backups, 1, ?MODULE:module_info(attributes)))).

backup_atom(Num) ->
  NumBackups = num_backups(),
  if Num > NumBackups -> erlang:exit(tried_to_create_too_many_backups) ;
     true -> list_to_atom(lists:flatten(io_lib:format("backup~p", [Num])))

  end.
