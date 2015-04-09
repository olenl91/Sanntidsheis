-module(cost).
-export([get_score/4]).

-define(NUMBER_OF_FLOORS, 4).
-define(TURN_COST, ?NUMBER_OF_FLOORS).


get_score(ElevatorFloor, up, OrderFloor, up) when OrderFloor >= ElevatorFloor ->
    OrderFloor - ElevatorFloor;
get_score(ElevatorFloor, up, OrderFloor, up) when OrderFloor < ElevatorFloor ->
    ?TURN_COST + ElevatorFloor - OrderFloor;

get_score(ElevatorFloor, down, OrderFloor, down) when OrderFloor =< ElevatorFloor ->
    ElevatorFloor - OrderFloor;
get_score(ElevatorFloor, down, OrderFloor, down) when OrderFloor > ElevatorFloor ->
    ?TURN_COST + OrderFloor - ElevatorFloor;

get_score(ElevatorFloor, up, OrderFloor, command) when OrderFloor >= ElevatorFloor ->
    OrderFloor - ElevatorFloor;
get_score(ElevatorFloor, up, OrderFloor, command) when OrderFloor < ElevatorFloor ->
    ?TURN_COST + ElevatorFloor - OrderFloor;

get_score(ElevatorFloor, down, OrderFloor, command) when OrderFloor =< ElevatorFloor ->
    ElevatorFloor - OrderFloor;
get_score(ElevatorFloor, down, OrderFloor, command) when OrderFloor > ElevatorFloor ->
    ?TURN_COST + OrderFloor - ElevatorFloor;

get_score(ElevatorFloor, up, OrderFloor, down) ->
    ?TURN_COST + abs(ElevatorFloor - OrderFloor);
get_score(ElevatorFloor, down, OrderFloor, up) ->
    ?TURN_COST + abs(ElevatorFloor - OrderFloor);

get_score(_ElevatorFloor, _ElevatorDirection, _OrderFloor, _OrderDirection) -> 
    erlang:error(badarg).
