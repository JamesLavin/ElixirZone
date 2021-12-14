-module(joe). 
-export([start/0, call/1]). 
-vsn(1.0).

call(Arg1) -> 
  io:fwrite("~p~n",[Arg1]). 

receive_loop() ->
  robert ! "Hello, Robert, from Joe!",
  receive
    Msg ->
      io:fwrite("Joe received a message: ~p~n",[Msg]),
      mike ! "Hello, Mike, from Joe!"
  end,
  receive
    Msg2 ->
      io:fwrite("Joe received a message: ~p~n",[Msg2])
  end.

start() ->
  Pid = spawn(fun() -> call("Joe has arrived"), receive_loop() end), 
  register(joe, Pid).
