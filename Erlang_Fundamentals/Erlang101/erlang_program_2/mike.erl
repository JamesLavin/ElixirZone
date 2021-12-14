-module(mike). 
-export([start/0, call/1]). 
-vsn(2.0).

call(Arg1) -> 
  io:fwrite("~p~n",[Arg1]). 

receive_loop() ->
  receive
    {Sender, Msg} ->
      io:fwrite("Mike 2.0 received a message from ~p: ~p~n",[Sender, Msg]),
      Sender ! {mike, "Hello from Mike 2.0!"}
  end.

start() ->
  Pid = spawn(fun() -> call("Mike 2.0 has arrived"), receive_loop() end), 
  register(mike, Pid).
