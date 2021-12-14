-module(unpopular). 
-export([start/0, call/1]). 

call(Arg1) -> 
  io:fwrite("~p~n",[Arg1]). 

receive_loop() ->
  receive
    {Sender, Msg} ->
      io:fwrite("Unpopular received a message: ~p~n",[Msg]),
      Sender ! {unpopular, "Hello from Unpopular! Am I popular now?"},
      receive_loop()
  after
    2000 -> io:fwrite("So long, cruel world!~n"), exit
  end.

start() ->
  Pid = spawn(fun() -> call("Unpopular has arrived"), receive_loop() end), 
  register(unpopular, Pid).
