-module(my_timer). 
-export([start/0, stop/0, display/1]). 

display(Arg1) -> 
  io:fwrite("~p~n",[Arg1]). 

loop() ->
  display(time()),
  erlang:send_after(1000, self(), next_tick),
  receive
    next_tick -> loop()
  end.

start() ->
  Pid = spawn(fun() -> display("Timer has started"), loop() end), 
  register(my_timer, Pid).

stop() ->
  exit(whereis(my_timer), kill).
