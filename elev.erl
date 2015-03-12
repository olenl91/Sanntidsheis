-module(elev).
-export([start/0]).

start() ->
    elev_driver:start(),
    fsm:start(),
    register(event_manager, spawn(fun() -> manage_events() end)).


%% husk at elevator_interface og fsm_interface ikke har samme PID og navn som elev/event_manager

manage_events() ->
    receive
	{floor_reached, Floor} ->
	    fsm:event_floor_reached(Floor);
	{set_motor_direction, Direction} ->
	    io:format("set motor dir ~n"),
	    elev_driver:set_motor_direction(Direction)
    end,
    manage_events().
			

    
