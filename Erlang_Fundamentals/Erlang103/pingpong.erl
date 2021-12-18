-module(pingpong).
-export([start/0, ping/0, pong/0]).

start()->
    PingPid = spawn(pingpong, ping, []),
    PongPid = spawn(pingpong, pong, []),
    PingPid!{PongPid, pong}.

ping()->
    receive
        {PongPid, pong}-> io:format("pong received~n"),
        PongPid!{self(), ping}
    end,
    ping().

pong()->
    receive
        {Pid, ping}-> io:format("ping received~n"),
        Pid!{self(), pong}
    end,
    pong().
