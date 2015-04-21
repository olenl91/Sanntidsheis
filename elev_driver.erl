-module(elev_driver).
-export([start/2, stop/0, init_port/2]).
-export([init/1, set_motor_direction/1, set_door_open_lamp/1, set_stop_lamp/1, set_floor_indicator/1, set_button_lamp/3]).

%% Module API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(simulator) -> call_port({elev_init, 1});
init(elevator) -> call_port({elev_init, 2}).

set_motor_direction(up) -> call_port({elev_set_motor_direction, 1});
set_motor_direction(down) -> call_port({elev_set_motor_direction, 2});
set_motor_direction(stop) -> call_port({elev_set_motor_direction, 0}).

set_door_open_lamp(on) -> call_port({elev_set_door_open_lamp, 1});
set_door_open_lamp(off) -> call_port({elev_set_door_open_lamp, 0}).

set_stop_lamp(on) -> call_port({elev_set_stop_lamp, 1});
set_stop_lamp(off) -> call_port({elev_set_stop_lamp, 0}).

set_floor_indicator(Floor) -> % maybe add some special case when Floor > 3, or crash!!!!!?
    call_port({elev_set_floor_indicator, Floor}).

%set_button_lamp(Floor, Direction, On)
set_button_lamp(Floor, up, true) -> call_port({elev_set_button_lamp, 0, Floor, 1}); % 6 cases is to much, improve this
set_button_lamp(Floor, up, false) -> call_port({elev_set_button_lamp, 0, Floor, 0});
set_button_lamp(Floor, down, true) -> call_port({elev_set_button_lamp, 1, Floor, 1});
set_button_lamp(Floor, down, false) -> call_port({elev_set_button_lamp, 1, Floor, 0});
set_button_lamp(Floor, command, true) -> call_port({elev_set_button_lamp, 2, Floor, 1});
set_button_lamp(Floor, command, false) -> call_port({elev_set_button_lamp, 2, Floor, 0}).

poll_order_buttons() -> call_port({poll_order_buttons}).

poll_floor_sensors() -> call_port({poll_floor_sensors}).

%% Call backs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
new_order(Listener, Direction, Floor) -> Listener ! {new_order, Direction, Floor}.
floor_reached(Listener, Floor) -> Listener ! {floor_reached, Floor}.

%% Process functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Listener, ElevatorType) ->
    spawn(?MODULE, init_port, ["../driver/elev_port", Listener]),
    timer:sleep(10),
    init(ElevatorType),
    spawn(fun() -> poll_everything() end).

stop() ->
    driver ! {call, self(), {stop}}.

init_port(ExtPrg, Listener) ->
    register(driver, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [{packet, 2}]),
    loop(Port, Listener).

call_port(Msg) ->
    driver ! {call, self(), Msg}.

loop(Port, Listener) ->
    receive
	{call, _Caller, Msg} ->
	    Port ! {self(), {command, encode(Msg)}},
	    loop(Port, Listener);
	{Port, {data, [11, 0, Floor]}} ->
	    new_order(Listener, up, Floor),
	    loop(Port, Listener);
	{Port, {data, [11, 1, Floor]}} ->
	    new_order(Listener, down, Floor),
	    loop(Port, Listener);
	{Port, {data, [11, 2, Floor]}} ->
	    new_order(Listener, command, Floor),
	    loop(Port, Listener);
	{Port, {data, [12, Floor]}} ->
	    floor_reached(Listener, Floor),
	    loop(Port, Listener);
	{Port, {data, [13]}} ->
	    Port ! {self(), close},
	    receive
		{Port, closed} ->
		    exit(normal)
	    end;
	{'EXIT', Port, _Reason} ->
	    exit(port_terminated)
    end.

poll_everything() ->
    poll_order_buttons(),
    poll_floor_sensors(),
    timer:sleep(10),
    poll_everything().

%% Helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
encode({poll_floor_sensors}) -> [12];
encode({stop}) -> [13].
