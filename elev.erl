-module(elev).
%-export([start/1]).
-compile(export_all).

-define(NUMBER_OF_FLOORS, 4). %should maybe be in other place?
-define(BUTTON_TYPES, [up, down, command]). %should maybe be some other place?


start(ElevatorType) ->
    DriverManagerPID = spawn(fun() -> driver_manager_init() end),
    FsmManagerPid = spawn(fun() -> fsm_manager_init() end),

    elev_driver:start(DriverManagerPID, ElevatorType),
    FsmPID = fsm:start(FsmManagerPid),
    register(fsm, FsmPID),

    QueuePID = queue:start(),
    register(queue, QueuePID).



fsm_manager_init() -> % dirty hack, plz fix
    timer:sleep(100), % wait for driver initalization
    queue:floor_reached(queue, 0), % dumb hack
    fsm_manager().
fsm_manager() ->
    receive
	{direction, request, Caller} ->
	    Direction = queue:get_next_direction(queue),
	    Caller ! {direction, response, Direction};
	{motor, up} ->
	    elev_driver:set_motor_direction(up),
	    queue:floor_left(queue, up);
	{motor, down} ->
	    elev_driver:set_motor_direction(down),
	    queue:floor_left(queue, down);
	{motor, stop} ->
	    elev_driver:set_motor_direction(stop);
	{doors, open} ->
	    queue:make_stop(queue),
	    elev_driver:set_door_open_lamp(on);
	{doors, close} ->
	    elev_driver:set_door_open_lamp(off)
    end,
    fsm_manager().

driver_manager_init() -> % more dirty tricks
    timer:sleep(100),
    driver_manager().
driver_manager() ->
    receive
	{new_order, Direction, Floor} ->
	    queue:add(queue, Floor, Direction),
	    fsm:event_new_order(fsm);
	{floor_reached, Floor} ->
	    fsm:event_floor_reached(fsm),
	    queue:floor_reached(queue, Floor)
    end,
    driver_manager().


%% Helper functions (should maybe not be helper functions in this module?)
%%%%%%%%%%%%%%%%%%%%%%%

    
%F(Floor)
foreach_floor(F) -> %should maybe (probably) me moved to somewhere else
    FloorIterator = fun(FloorIterator, Floor) ->
			    if 
				Floor == 0 ->
				    F(Floor);
				(Floor > 0) and (Floor =< ?NUMBER_OF_FLOORS-1) ->
				    F(Floor),
				    FloorIterator(FloorIterator, Floor-1)
			    end
		    end,
    
    FloorIterator(FloorIterator, ?NUMBER_OF_FLOORS-1),
    ok.
