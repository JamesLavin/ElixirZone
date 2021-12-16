-module(sleeper).
-behaviour(gen_statem).

-export([start/0,push/0,alarm/0,bedtime/0,say_hi/0,get_count/0,stop/0]).
-export([terminate/3,code_change/4,init/1,callback_mode/0]).
-export([awake/3,asleep/3]).

name() -> statem_sleeper. % The registered server name

%% API.  This example uses a registered name name()
%% and does not link to the caller.
start() ->
    gen_statem:start({local,name()}, ?MODULE, [], []).
push() ->
    gen_statem:call(name(), push).
say_hi() ->
    gen_statem:call(name(), say_hi).
alarm() ->
    gen_statem:call(name(), alarm).
bedtime() ->
    gen_statem:call(name(), bedtime).
get_count() ->
    gen_statem:call(name(), get_count).
stop() ->
    gen_statem:stop(name()).

%% Mandatory callback functions
terminate(_Reason, _State, _Data) ->
    void.
code_change(_Vsn, State, Data, _Extra) ->
    {ok,State,Data}.
init([]) ->
    %% Set the initial state + data.  Data is used only as a counter.
    State = asleep, Data = 0,
    {ok,State,Data}.
callback_mode() -> state_functions.

%%% state callback(s)

asleep({call,From}, push, Data) ->
    %% Go to 'awake', increment count and reply
    %% that the resulting status is 'awake'
    {next_state,awake,Data+1,[{reply,From,awake}]};
asleep({call,From}, say_hi, Data) ->
    {next_state,asleep,Data,[{reply,From,"Zzzzzz..."}]};
asleep({call,From}, alarm, Data) ->
    {next_state,awake,Data,[{reply,From,"I'm up!"}]};
%asleep({call,_From}, bedtime, Data) ->
%    {next_state,asleep,Data,[]};
asleep(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

awake({call,From}, push, Data) ->
    %% Go to 'asleep' and reply that the resulting status is 'asleep'
    {next_state,asleep,Data,[{reply,From,asleep}]};
awake({call,From}, say_hi, Data) ->
    {next_state,awake,Data,[{reply,From,"Hi, there!"}]};
%awake({call,_From}, alarm, Data) ->
%    {next_state,awake,Data,[]};
awake({call,From}, bedtime, Data) ->
    {next_state,asleep,Data,[{reply,From,"I'm going to bed, Mom!"}]};
awake(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

%% Handle events common to all states
handle_event({call,From}, get_count, Data) ->
    %% Reply with the current count
    {keep_state,Data,[{reply,From,Data}]};
handle_event({call,From}, _, Data) ->
    %% Ignore all other events
    {keep_state,Data,[{reply,From,"..."}]};
handle_event(_, _, Data) ->
    %% Ignore all other events
    {keep_state,Data}.
