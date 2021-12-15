-module(safe7).

-export([start/0, counter_loop/2, start_counter_loop/1, backup_loop/1, create_or_update_backup/2,
         create_backups/0, create_backup_unless_exists/1, send_with_retries/5]).

-num_backups(2).

start_counter_loop({Backup1Ref, Backup1Pid}) ->
  send_with_retries(backup1, {state_request, self()}, 5, 1000, 5000),
  % backup1 ! {state_request, self()},
  receive
    {current_state, Count} ->
      counter_loop({Backup1Ref, Backup1Pid}, Count)
  end.

counter_loop({Backup1Ref, Backup1Pid}, Count) ->
  timer:sleep(1000),
  NewCount = Count + 1,
  create_or_update_backup(backup1, NewCount),
  io:format("Count is now ~p~n", [NewCount]),
  counter_loop({Backup1Ref, Backup1Pid}, NewCount).

backup_loop({BackupNum, Count}) ->
  receive
    {new_state, NewCount} ->
      NumBackups = num_backups(),
      if BackupNum < NumBackups ->
          SendTo = backup_atom(BackupNum + 1),
          SendTo ! {new_state, NewCount};
        true -> ok
      end,
      backup_loop({BackupNum, NewCount});
    {state_request, Pid} ->
      Pid ! {current_state, Count},
      backup_loop({BackupNum, Count})
  end.

start() ->
  {Backup1Ref, Backup1Pid} = create_backups(),
  Pid = spawn(?MODULE, start_counter_loop, [{Backup1Ref, Backup1Pid}]),
  register(?MODULE, Pid).

% create a cache/backup process to hold state for fault-recovery
create_backup_unless_exists(BackupNum, InitialCount) ->
  BackupAtom = backup_atom(BackupNum),                    % backup1, backup2, etc.
  BackupExists = is_pid(whereis(BackupAtom)),             % true or false
  NumBackups = num_backups(),                             % 1, 2, 3, etc.
  if BackupExists ->
       BackupRef = monitor(process, BackupAtom),
       {nil, BackupRef};
     true ->
       {BackupPid, BackupRef} = spawn_monitor(?MODULE, backup_loop, [{BackupNum, InitialCount}]),
       true = register(BackupAtom, BackupPid),
       if BackupNum < NumBackups ->
         create_backup_unless_exists(BackupNum + 1, InitialCount);
	 true -> ok
       end,
       {BackupPid, BackupRef}
  end.

create_backup_unless_exists(BackupNum) ->
  create_backup_unless_exists(BackupNum, 0).

create_backups() ->
  create_backup_unless_exists(1).

create_or_update_backup(BackupAtom, Count) ->
  BackupExists = is_pid(whereis(BackupAtom)),
  if BackupExists ->
       BackupAtom ! {new_state, Count};
     true ->
       {BackupPid, Ref} = spawn_monitor(?MODULE, backup_loop, [{BackupAtom, Count}]),
       register(BackupAtom, BackupPid)
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
