-module(my_toggled_timer). 
-export([start/0, stop/0, display/1, toggle/0, maybe_schedule_next_tick/1, maybe_display_time/1]). 

display(Arg1) -> 
  io:fwrite("~p~n",[Arg1]). 

loop(State) ->
  NewState = maybe_schedule_next_tick(State),
  maybe_display_time(NewState),
  receive
    next_tick ->
      EvenNewerState = maps:update(next_tick_ref, nil, NewState),
      loop(EvenNewerState);
    toggle_display ->
      CurrentDisplay = maps:get(display, NewState),
      EvenNewerState = maps:update(display, not CurrentDisplay, NewState),
      loop(EvenNewerState)
  end.

toggle() ->
  my_timer ! toggle_display.

maybe_display_time(#{display := true}) ->
  display(time());
maybe_display_time(#{display := false}) ->
  ok.

maybe_schedule_next_tick(#{next_tick_ref := nil} = State) ->
  NextTickRef = erlang:send_after(1000, self(), next_tick),
  maps:update(next_tick_ref, NextTickRef, State);
maybe_schedule_next_tick(State) ->
  State.

start() ->
  State = initial_state(),
  Pid = spawn(fun() -> display("Timer has started"), loop(State) end), 
  register(my_timer, Pid).

initial_state() ->
  #{display => true, next_tick_ref => nil}.

stop() ->
  exit(whereis(my_timer), kill).
