-module(mike). 
-export([start/0, call/1]). 
-vsn(3.0).

call(Arg1) -> 
  io:fwrite("~p~n",[Arg1]). 

receive_loop() ->
  receive
    {joe, Msg} ->
      io:fwrite("Mike received a message from Joe: ~p~n",[Msg]),
      joe ! {mike, "Hello, Joe, from Mike!"},
      receive_loop();
    {robert, Msg} ->
      io:fwrite("Mike received a message from Robert: ~p~n",[Msg]),
      robert ! {mike, "Hello, Robert, from Mike!"},
      receive_loop();
    {Sender, Msg} ->
      io:fwrite("Mike received a message from a mystery sender: ~p~n",[Msg]),
      Sender ! {mike, "Hello from Mike!"},
      receive_loop()
  end.

start() ->
  Pid = spawn(fun() -> call("Mike has arrived"), receive_loop() end), 
  register(mike, Pid).
