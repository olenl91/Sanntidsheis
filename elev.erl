-module(elev).
-export([start/0]).

start() ->
    elev_driver:start(),
    fsm:start(),
    register(event_manager, spawn(fun() -> manage_events() end)).



manage_events() ->
    receive
	{floor_reached, Floor} ->
	    fsm:event_floor_reached(Floor);
	{set_motor_direction, Direction} ->
	    io:format("set motor dir ~n"),
	    elev_driver:set_motor_direction(Direction);
	{fsm_info, stopped, 0} ->
	    fsm:go_direction(up);
	{fsm_info, stopped, 3} ->
	    fsm:go_direction(down);
	{fsm_info, stopped, Floor} ->
	    fsm:go_direction(up)
    end,
    manage_events().
			

    
