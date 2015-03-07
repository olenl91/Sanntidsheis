-module(elev_driver).
-export([start/0, stop/0, init_port/1]).
-export([init/1, set_motor_direction/1, set_door_open_lamp/1, set_stop_lamp/1, set_floor_indicator/1, set_button_lamp/3]).


start() ->
    register(caller, self()),
    spawn(?MODULE, init_port, ["driver/elev_port"]),
    timer:sleep(10),
    init(simulator),
    spawn(fun() -> poll_everything() end).
		  
stop() ->
    driver ! stop.


poll_everything() ->
    poll_order_buttons(),
    poll_floor_sensors(),
    timer:sleep(10),
    poll_everything().


init(simulator) ->
    call_port({elev_init, 1});
init(elevator) ->
    call_port({elev_init, 2}).

set_motor_direction(up) ->
    call_port({elev_set_motor_direction, 1});
set_motor_direction(down) ->
    call_port({elev_set_motor_direction, 2});
set_motor_direction(stop) ->
    call_port({elev_set_motor_direction, 0}).

set_door_open_lamp(on) ->
    call_port({elev_set_door_open_lamp, 1});
set_door_open_lamp(off) ->
    call_port({elev_set_door_open_lamp, 0}).

set_stop_lamp(on) ->
    call_port({elev_set_stop_lamp, 1});
set_stop_lamp(off) ->
    call_port({elev_set_stop_lamp, 0}).

set_floor_indicator(Floor) -> % maybe add some special case when Floor > 3, or crash!!!!!?
    call_port({elev_set_floor_indicator, Floor}).

set_button_lamp(Button, Floor, Value) ->
    call_port({elev_set_button_lamp, Button, Floor, Value}).

poll_order_buttons() ->
    call_port({poll_order_buttons}).

poll_floor_sensors() ->
    call_port({poll_floor_sensors}).


call_port(Msg) ->
    driver ! {call, self(), Msg}.

init_port(ExtPrg) ->
    register(driver, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [{packet, 2}]),
    loop(Port).

loop(Port) ->
    receive
	{call, Caller, Msg} ->
	    Port ! {self(), {command, encode(Msg)}},
	    loop(Port); 
	{Port, {data, [11, 0, Floor]}} ->
	    caller ! {new_order, up, Floor},
	    loop(Port);
	{Port, {data, [11, 1, Floor]}} ->
	    caller ! {new_order, down, Floor},
	    loop(Port);
	{Port, {data, [11, 2, Floor]}} ->
	    caller ! {new_order, command, Floor},
	    loop(Port);
	{Port, {data, [12, Floor]}} ->
	    caller ! {floor_reached, Floor},
	    loop(Port);
	stop ->
	    Port ! {self(), close},
	    receive
		{Port, closed} ->
		    exit(normal)
	    end;
	{'EXIT', Port, Reason} ->
	    exit(port_terminated)
    end.

encode({elev_init, Type}) -> [1, Type];
encode({elev_set_motor_direction, Dirn}) -> [2, Dirn];
encode({elev_set_door_open_lamp, Value}) -> [3, Value];
encode({elev_get_obstruction_signal}) -> [4];
encode({elev_get_stop_signal}) -> [5];
encode({elev_set_stop_lamp, Value}) -> [6, Value];
encode({elev_get_floor_sensor_signal}) -> [7];
encode({elev_set_floor_indicator, Floor}) -> [8, Floor];
encode({elev_get_button_signal, Button, Floor}) -> [9, Button, Floor];
encode({elev_set_button_lamp, Button, Floor, Value}) -> [10, Button, Floor, Value];
encode({poll_order_buttons}) -> [11];
encode({poll_floor_sensors}) -> [12].
    

