-module(fsm).
-compile(export_all).
%-export([start/0, go_direction/1, event_floor_reached/1]).

%% Module Interface
%%%%%%%%%%%%%%%%%%%%

go_direction(Pid, up) ->
    Pid ! up;
go_direction(Pid, down) ->
    Pid ! down;
go_direction(Pid, open) ->
    Pid ! open.

event_floor_reached(Pid) ->
    Pid ! floor_reached.


%% Process Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


start(Listener) ->
    spawn(fun() -> init(Listener) end).
    

%% States
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


init(Listener) ->
    Listener ! {motor, up},
    receive
	floor_reached ->
	    idle(Listener)
    end.

driving_up(Listener) ->
    Listener ! {motor, up},
    receive
	floor_reached ->
	    idle(Listener)
    end.


driving_down(Listener) ->
    Listener ! {motor, down},
    receive
        floor_reached ->
	    idle(Listener)
    end.


idle(Listener) ->
    Listener ! {motor, stop},
    receive
	up ->
	    driving_up(Listener);
	down ->
	    driving_down(Listener);
	open ->
	    open_doors(Listener)		
    end.

open_doors(Listener) ->
    Listener ! {doors, open},
    timer:sleep(3000),
    Listener ! {doors, close},
    idle(Listener).

