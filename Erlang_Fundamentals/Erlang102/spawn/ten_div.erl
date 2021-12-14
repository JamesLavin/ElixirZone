-module(ten_div).
-export([start_supervisor/0, sup_loop/0, start_server/0, request/1, loop/0]).

% client function that starts the ten_div_sup supervisor process and registers it
start_supervisor() ->
    TenDivSupervisorPid = spawn(ten_div, sup_loop, []),
    register(ten_div_sup, TenDivSupervisorPid),
    ok.

sup_loop() ->
    process_flag(trap_exit, true),
    {ok, TenDivPid} = start_server(),
    receive
        {'EXIT', TenDivPid, _Reason} ->
            sup_loop()
    end.

% client function that starts the ten_div process, registers it, and links to it
start_server() ->
    process_flag(trap_exit, true),
    TenDivPid = spawn_link(ten_div, loop, []),
    register(ten_div, TenDivPid),
    {ok, TenDivPid}.

% client function to send request to ten_div process and interpret the response
request(Int) ->
    ten_div ! {request, self(), Int},
    receive
        {result, Result} -> Result;
        {'EXIT', _Pid, Reason} -> {error, Reason}
    after 1000 -> timeout
    end.

% server function invoked by ten_div
loop() ->
    receive
        {request, Pid, Msg} ->
            Pid ! {result, 10 / Msg}
    end,
    loop().
