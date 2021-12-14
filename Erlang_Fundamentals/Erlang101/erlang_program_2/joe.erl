-module(joe). 
-export([start/0, call/1]). 
-vsn(2.0).

call(Arg1) -> 
  io:fwrite("~p~n",[Arg1]). 

receive_loop() ->
  robert ! {joe, "Hello, Robert 2.0, from Joe 2.0!"},
  receive
    {Sender, Msg} ->
      io:fwrite("Joe 2.0 received a message from ~p: ~p~n",[Sender, Msg]),
      mike ! {joe, "Hello from Joe 2.0!"}
  end,
  receive
    {Sender2, Msg2} ->
      io:fwrite("Joe 2.0 received a message from ~p: ~p~n",[Sender2, Msg2])
  end.

start() ->
  Pid = spawn(fun() -> call("Joe 2.0 has arrived"), receive_loop() end), 
  register(joe, Pid).
