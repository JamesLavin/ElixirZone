-module(prop_reversibility).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_test() ->
    ?FORALL({Num, Denom}, {numerator(), denominator()},
        begin
            is_number(Num / Denom)
        end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
boolean(_) -> true.

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
numerator() -> integer().

% This version won't catch the error because the inputs don't include 0
% This is a reason why business logic bugs can be missed by ordinary example-based testing
% denominator() -> oneof([range(1,10), range(11,100), range(101, 1000), range(1001,10000)]).

% This version will (eventually) catch the error after hitting the test with more and
% more inputs over time:
denominator() -> oneof([range(1,10), range(11,100), range(101, 1000), range(1001,10000), integer()]).
