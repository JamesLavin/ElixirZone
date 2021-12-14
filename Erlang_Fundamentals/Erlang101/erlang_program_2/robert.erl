-module(robert). 
-export([start/0, call/1]). 
-vsn(2.0).

call(Arg1) -> 
  io:fwrite("~p~n",[Arg1]). 

receive_loop() ->
  receive
    {Sender, Msg} ->
      io:fwrite("Robert 2.0 received a message from ~p: ~p~n",[Sender, Msg]),
      Sender ! {robert, "Hello from Robert 2.0"}
  end.

start() ->
  Pid = spawn(fun() -> call("Robert 2.0 has arrived"), receive_loop() end), 
  register(robert, Pid).
