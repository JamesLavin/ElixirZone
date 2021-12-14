-module(robert). 
-export([start/0, call/1]). 
-vsn(1.0).

call(Arg1) -> 
  io:fwrite("~p~n",[Arg1]). 

receive_loop() ->
  receive
    Msg ->
      io:fwrite("Robert received a message: ~p~n",[Msg]),
      joe ! "Hello, Joe, from Robert"
  end.

start() ->
  Pid = spawn(fun() -> call("Robert has arrived"), receive_loop() end), 
  register(robert, Pid).
