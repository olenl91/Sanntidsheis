-module(fsm).
-export([start/0, go_direction/1, event_floor_reached/1]).

start() ->
    register(fsm, spawn(fun() -> init() end)).
%    spawn(?MODULE, init).
    

go_direction(up) ->
    fsm ! {go, up};
go_direction(down) ->
    fsm ! {go, down};
go_direction(open) ->
    fsm ! {go, open}.

event_floor_reached(Floor) ->
    fsm ! {floor_reached, Floor}.



init() ->
    event_manager ! {set_motor_direction, up},
    receive
	{floor_reached, Floor} ->
	    idle(Floor)
    end.

driving_up(LastFloor) ->
    event_manager ! {set_motor_direction, up},
    receive
	{floor_reached, Floor} ->
	    idle(Floor)
    end.


driving_down(LastFloor) ->
    event_manager ! {set_motor_direction, down},
    receive
        {floor_reached, Floor} ->
	    idle(Floor)
						%lol
    end.


idle(ThisFloor) ->
						% request order from queue, every now and then,
    event_manager ! {set_motor_direction, stop},
    event_manager ! {fsm_info, stopped, ThisFloor},
						% set floor indicato
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

