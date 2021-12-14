-module(robert). 
-export([start/0, call/1]). 
-vsn(3.0).

call(Arg1) -> 
  io:fwrite("~p~n",[Arg1]). 

receive_loop() ->
  receive
    {joe, Msg} ->
      io:fwrite("Robert received a message from Joe: ~p~n",[Msg]),
      joe ! {robert, "Hello, Joe, from Robert"},
      receive_loop();
    {mike, Msg} ->
      io:fwrite("Robert received a message from Mike: ~p~n",[Msg]),
      mike ! {robert, "Hello, Mike, from Robert"},
      receive_loop();
    {Sender, Msg} ->
      io:fwrite("Robert received a message from an unknown person: ~p~n",[Msg]),
      Sender ! {robert, "Hello from Robert"},
      receive_loop()
  end.

start() ->
  Pid = spawn(fun() -> call("Robert has arrived"), receive_loop() end), 
  register(robert, Pid).
