-module(joe). 
-export([start/0, call/1]). 
-vsn(3.0).

call(Arg1) -> 
  io:fwrite("~p~n",[Arg1]),
  robert ! {joe, "Hello, Robert, from Joe!"}.

receive_loop() ->
  receive
    {robert, Msg} ->
      io:fwrite("Joe received a message from Robert: ~p~n",[Msg]),
      mike ! {joe, "Hello from Joe!"},
      receive_loop();
    {mike, Msg2} ->
      io:fwrite("Joe received a message from Mike: ~p~n",[Msg2]),
      receive_loop();
    {_Unknown, Msg2} ->
      io:fwrite("Joe received a message from someone else: ~p~n",[Msg2]),
      receive_loop()
  end.

start() ->
  Pid = spawn(fun() -> call("Joe has arrived"), receive_loop() end), 
  register(joe, Pid).
