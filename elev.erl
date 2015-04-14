-module(elev).
-export([start/0]).

start() ->
    DriverManagerPID = spawn(fun() -> driver_manager_init() end),
    FsmManagerPid = spawn(fun() -> fsm_manager_init() end),

    elev_driver:start(DriverManagerPID),
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
	    queue:add(queue, Floor, Direction);
	{floor_reached, Floor} ->
	    fsm:event_floor_reached(fsm),
	    queue:floor_reached(queue, Floor)
    end,
    driver_manager().
