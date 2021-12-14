-module(add_three).
-export([start_supervisor/0, sup_loop/0, start_server/0, request/1, loop/0]).

% client function that starts the add_three_sup supervisor process and registers it
start_supervisor() ->
    AddThreeSupervisorPid = spawn(add_three, sup_loop, []),
    register(add_three_sup, AddThreeSupervisorPid),
    ok.

sup_loop() ->
    process_flag(trap_exit, true),
    {ok, AddThreePid} = start_server(),
    receive
        {'EXIT', AddThreePid, _Reason} ->
            sup_loop()
    end.

% client function that starts the add_three process, registers it, and links to it
start_server() ->
    process_flag(trap_exit, true),
    AddThreePid = spawn_link(add_three, loop, []),
    register(add_three, AddThreePid),
    {ok, AddThreePid}.

% client function to send request to add_three process and interpret the response
request(Int) ->
    add_three ! {request, self(), Int},
    receive
        {result, Result} -> Result;
        {'EXIT', _Pid, Reason} -> {error, Reason}
    after 1000 -> timeout
    end.

% server function invoked by add_three
loop() ->
    receive
        {request, Pid, Msg} ->
            Pid ! {result, Msg + 3}
    end,
    loop().
