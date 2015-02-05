-module(elev).
-export([start/0, stop/0, init/1]).
-export([elev_init/1, elev_set_motor_direction/1, elev_set_door_open_lamp/1, elev_get_obstruction_signal/0, elev_get_stop_signal/0, elev_set_stop_lamp/1, elev_get_floor_sensor_signal/0, elev_set_floor_indicator/1, elev_get_button_signal/2, elev_set_button_lamp/3]).

start() ->
    spawn(?MODULE, init, ["driver/elev_port"]).
stop() ->
    complex ! stop.

elev_init(simulator) ->
    call_port({elev_init, 1});
elev_init(elevator) ->
    call_port({elev_init, 2}).

elev_set_motor_direction(Dirn) ->
    call_port({elev_set_motor_direction, Dirn}).

elev_set_door_open_lamp(Value) ->
    call_port({elev_set_door_open_lamp, Value}).

elev_get_obstruction_signal() ->
    call_port({elev_get_obstruction_signal}).

elev_get_stop_signal() ->
    call_port({elev_get_stop_signal}).

elev_set_stop_lamp(Value) ->
    call_port({elev_set_stop_lamp, Value}).

elev_get_floor_sensor_signal() ->
    call_port({elev_get_floor_sensor_signal}).

elev_set_floor_indicator(Floor) ->
    call_port({elev_set_floor_indicator, Floor}).

elev_get_button_signal(Button, Floor) ->
    call_port({elev_get_button_signal, Button, Floor}).

elev_set_button_lamp(Button, Floor, Value) ->
    call_port({elev_set_button_lamp, Button, Floor, Value}).



call_port(Msg) ->
	complex ! {call, self(), Msg},
    	receive
		{complex, Result} ->
	    	Result
    	end.

init(ExtPrg) ->
    register(complex, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [{packet, 2}]),
    loop(Port).

loop(Port) ->
    receive
	{call, Caller, Msg} ->
	    Port ! {self(), {command, encode(Msg)}},
	    receive
		{Port, {data, Data}} ->
		    Caller ! {complex, decode(Data)}
	    end,
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
encode({elev_set_button_lamp, Button, Floor, Value}) -> [10, Button, Floor, Value].


decode([Int]) -> Int.

