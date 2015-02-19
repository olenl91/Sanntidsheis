-module(fsm).
-export([start/0, go_direction/1]).

start() ->
    spawn(fun() -> poll_sensor(255) end),
    register(elevator, spawn(fun() -> init() end)).


poll_sensor(LastFloor) ->
    case elev:elev_get_floor_sensor_signal() of
	LastFloor ->
	    poll_sensor(LastFloor);
	ThisFloor ->
	    floor_reached(ThisFloor),
	    timer:sleep(50),
	    poll_sensor(ThisFloor)
    end.



go_direction(up) ->
    elevator ! {go, up};
go_direction(down) ->
    elevator ! {go, down};
go_direction(open) ->
    elevator ! {go, open}.


floor_reached(255) ->
    ok;
floor_reached(Floor) ->    
    io:format("Floor is reached ~B ~n", [Floor]),
    elevator ! {floor_reached, Floor}.
    



init() ->
    driving_up(0),
    receive
	{floor_reached, Floor} ->
	    idle(Floor)
    end.

driving_up(LastFloor) ->
    elev:elev_set_motor_direction(1),
    receive
	{floor_reached, Floor} ->
	    idle(Floor)
    end.


driving_down(LastFloor) ->
    elev:elev_set_motor_direction(2),
    receive
        {floor_reached, Floor} ->
	    idle(Floor)
						%lol
    end.


idle(ThisFloor) ->
						% request order from queue, every now and then,
    elev:elev_set_motor_direction(0),
    elev:elev_set_floor_indicator(ThisFloor),
    receive
	{go, up} ->
	    driving_up(ThisFloor);
	{go, down} ->
	    driving_down(ThisFloor);
	{go, open} ->
	    open_doors(ThisFloor)

    end.

open_doors(ThisFloor) ->
    elev:elev_set_door_open_lamp(1),
    timer:sleep(3000),
    elev:elev_set_door_open_lamp(0),
    idle(ThisFloor).
