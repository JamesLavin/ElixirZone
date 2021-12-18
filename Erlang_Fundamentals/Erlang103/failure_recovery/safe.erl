-module(safe).

-export([start/0, restart/0, counter_loop/2, start_counter_loop/0, 
	 backup_loop/2, start_backup_loop/1, create_or_update_backup/2,
         create_backup1_unless_exists/0, create_backup1/1,
	 create_backup_unless_exists/1, create_backup_unless_exists/2, send_with_retries/5]).

-num_backups(2).

% Client command to launch safe, which will launch its own backup1
start() ->
  {LoopPid, _LoopRef} = spawn_monitor(?MODULE, start_counter_loop, []),
  register(?MODULE, LoopPid).

restart() ->
  io:format("~p is restarting ~p~n", [self(), ?MODULE]),
  {LoopPid, LoopRef} = spawn_monitor(?MODULE, start_counter_loop, []),
  register(?MODULE, LoopPid),
  {LoopPid, LoopRef}.

start_counter_loop() ->
  io:format("~p (PID: ~p) running start_counter_loop~n", [?MODULE, self()]),
  io:format("backup1 PID: ~p~n", [whereis(backup1)]),
  {Backup1Pid, Backup1Ref} = create_backup1_unless_exists(),
  send_with_retries(Backup1Pid, {state_request, self()}, 5, 1000, 5000),  % e.g., backup1 ! {state_request, self()},
  receive
    {current_state, Count} ->
      io:format("~p (PID: ~p) start_counter_loop received current_state update of ~p~n", [?MODULE, self(), Count]),
      counter_loop({Backup1Pid, Backup1Ref}, Count);
    {'DOWN', Backup1Ref, process, Backup1Pid, _Reason} ->
      io:format("~p (PID: ~p) received DOWN message for ~p (~p) BEFORE counter_loop~n", [?MODULE, self(), backup1, Backup1Pid]),
      start_counter_loop()
  end.

counter_loop({Backup1Pid, Backup1Ref}, Count) ->
  receive
    {current_state, NewCount} ->
      io:format("~p counter_loop received current_state update of ~p~n", [self(), NewCount]),
      counter_loop({Backup1Pid, Backup1Ref}, NewCount);
    {'DOWN', Backup1Ref, process, Backup1Pid, _Reason} ->
      io:format("~p counter received DOWN message for ~p (~p)~n", [self(), backup1, Backup1Pid]),
      {NewBackup1Pid, NewBackup1Ref} = create_backup1(Count),
      counter_loop({NewBackup1Pid, NewBackup1Ref}, Count);
    _Other ->
      io:format("Counter received unexpected message~n", [])
  after 1000 ->
      ok
      % io:format("backup1 Pid: ~p~n", [whereis(backup1)])
  end,
  IncrementedCount = Count + 1,
  create_or_update_backup(backup1, IncrementedCount),
  io:format("Count in ~p (PID: ~p) is now ~p~n", [?MODULE, self(), IncrementedCount]),
  counter_loop({Backup1Pid, Backup1Ref}, IncrementedCount).

% Each process monitors another process:
%   safe monitors backup1
%   backup1 monitors backup2
%   backup(n) monitors backup(n+1)
%   backupN (where N is the number of backups) monitors the main process, safe
% When safe starts, it creates backup1, then monitors it
% When backup(n) starts, it creates backup(n+1), then monitors it
% When backupN starts, it simply monitors safe, which is already running
start_backup_loop({BackupNum, Count}) ->
  io:format("backup~p (PID: ~p) starting backup loop with count ~p~n", [BackupNum, self(), Count]),
  NumBackups = num_backups(),
   if
     BackupNum < NumBackups ->
       % I am not the last backup, so my responsibility is starting and monitoring my backup, BackupNum + 1
       {MyBackupPid, MyBackupRef} = create_backup_unless_exists(BackupNum + 1, Count);
     BackupNum == NumBackups ->
       % I am the last backup, so my responsibility is monitoring the main process, safe
       io:format("Last backup (PID: ~p) is calling monitor_safe~n", [self()]),
       {MyBackupPid, MyBackupRef} = monitor_safe();
     true ->
       MyBackupRef = nil,
       MyBackupPid = nil
   end,
   backup_loop({MyBackupPid, MyBackupRef}, {BackupNum, Count}).

backup_loop({MyBackupPid, MyBackupRef}, {BackupNum, Count}) ->
  NumBackups = num_backups(),
  receive
    {new_state, NewCount} ->
      io:format("backup~p (PID: ~p) received new_state of ~p~n", [BackupNum, self(), NewCount]),
      if
        BackupNum < NumBackups ->
          MyBackup = backup_atom(BackupNum + 1),
          MyBackup ! {new_state, NewCount};
        true ->
          ok
      end,
      backup_loop({MyBackupPid, MyBackupRef}, {BackupNum, NewCount});
    {state_request, Pid} ->
      Pid ! {current_state, Count},
      backup_loop({MyBackupPid, MyBackupRef}, {BackupNum, Count});
    {'DOWN', MyBackupRef, process, MyBackupPid, _Reason} ->
      io:format("backup~p received DOWN message for PID (~p)~n", [BackupNum, MyBackupPid]),
      if
        BackupNum == NumBackups ->
          % I am the last backup in the chain and what crashed is the main process, safe, which I must restart
          {NewMyBackupPid, NewMyBackupRef} = restart(),
          backup_loop({NewMyBackupPid, NewMyBackupRef}, {BackupNum, Count});
        true ->
          % I am not the last backup in the chain and what crashed is my backup (BackupNum + 1), which I must restart
          % BackupAtom = backup_atom(BackupNum + 1),           % BackupNum + 1 because we're restarting the NEXT backup
          {NewMyBackupPid, NewMyBackupRef} = create_backup_unless_exists(BackupNum + 1, Count),
          backup_loop({NewMyBackupPid, NewMyBackupRef}, {BackupNum, Count})
      end
  end.

monitor_safe() ->
  SafePid = whereis(safe),
  AlreadyExists = is_pid(SafePid),
  if
    AlreadyExists ->
      SafeRef = monitor(process, SafePid),
      io:format("Last backup (~p) is monitoring safe (~p)~n", [self(), SafePid]),
      {SafePid, SafeRef};
    true ->
      ok
  end.

% create a cache/backup process to hold state for fault-recovery
create_backup_unless_exists(BackupNum, InitialCount) ->
  MyBackupAtom = backup_atom(BackupNum),                    % backup1, backup2, etc.
  MyBackupPid = whereis(MyBackupAtom),
  MyBackupExists = is_pid(MyBackupPid),             % true or false
  io:format("~p checking whether ~p (Pid: ~p) already exists?... ~p~n", [self(), MyBackupAtom, MyBackupPid, MyBackupExists]),
  if MyBackupExists ->
       io:format("~p not creating backup ~p because it already exists~n", [self(), MyBackupAtom]),
       MyBackupRef = monitor(process, MyBackupAtom),
       {MyBackupPid, MyBackupRef};
     true ->
       io:format("~p creating ~p with value ~p~n", [self(), MyBackupAtom, InitialCount]),
       {NewMyBackupPid, NewMyBackupRef} = spawn_monitor(?MODULE, start_backup_loop, [{BackupNum, InitialCount}]),
       % unregister(MyBackupAtom),
       true = register(MyBackupAtom, NewMyBackupPid),
       {NewMyBackupPid, NewMyBackupRef}
  end.

create_backup_unless_exists(BackupNum) ->
  create_backup_unless_exists(BackupNum, 0).

create_backup1_unless_exists() ->
  io:format("~p running create_backup1_unless_exists. Backup1 Pid: ~p~n", [self(), whereis(backup1)]),
  create_backup_unless_exists(1, 0).

create_backup1(Count) ->
  create_backup_unless_exists(1, Count).

create_or_update_backup(BackupAtom, Count) ->
  BackupPid = whereis(BackupAtom),
  BackupExists = is_pid(BackupPid),
  if BackupExists ->
       % BackupRef = monitor(process, BackupPid),
       BackupPid ! {new_state, Count};
     true ->
       {MyBackupPid, MyBackupRef} = spawn_monitor(?MODULE, backup_loop, [{BackupAtom, Count}]),
       true = register(BackupAtom, MyBackupPid),
       {MyBackupPid, MyBackupRef}
  end.

num_backups() ->
  lists:nth(1, element(2, lists:keyfind(num_backups, 1, ?MODULE:module_info(attributes)))).

backup_atom(Num) ->
  NumBackups = num_backups(),
  if Num > NumBackups -> erlang:exit(tried_to_create_too_many_backups) ;
     true -> list_to_atom(lists:flatten(io_lib:format("backup~p", [Num])))
  end.

send_with_retries(To, Message, 0, _, _) ->
    To ! Message;
send_with_retries(To, Message, RetriesLeft, RetryDelayMs, MaxRetryDelayMs) ->
    try
        To ! Message
    catch
        % registered process To is unavailable
        exit: {badarg, {To, Message}} ->
            io:format("catch: exit: {badarg,{~w, ~w}}~n", [To, Message]), % dbg only
            % retry after RetryDelayMs milliseconds
            timer:sleep(min(RetryDelayMs, MaxRetryDelayMs)),
            send_with_retries(To, Message, RetriesLeft - 1, 2 * RetryDelayMs, MaxRetryDelayMs)
    end.
