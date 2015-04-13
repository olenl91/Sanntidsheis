-module(queue).
-compile(export_all).
%-export(add_order/2, get/1, remove/2, get_next_order/2]).

-record(order, {floor, direction}).
-record(schedule, {orders = [], elevator_next_floor, elevator_direction}).

-define(NUMBER_OF_FLOORS, 4).
-define(TURN_COST, ?NUMBER_OF_FLOORS).



%% module interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add(Pid, OrderFloor, OrderDirection) ->
    Pid ! {add_order, OrderFloor, OrderDirection, self()}, 
    receive
	ok ->
	    ok
    end.

remove(Pid, OrderFloor, OrderDirection) ->
    Pid ! {remove_order, OrderFloor, OrderDirection, self()},
    receive
	ok ->
	    ok
    end.

get_next_floor(Pid) ->
    Pid ! {get_next_floor, self()},
    receive
	{floor, Floor} ->
	    Floor
    end.

floor_reached(Pid, Floor) ->
    Pid ! {floor_reached, Floor, self()},
    receive 
	ok ->
	    ok
    end.

floor_left(Pid, Direction) -> 
    Pid ! {floor_left, Direction, self()},
    receive
	ok ->
	    ok
    end.

get_schedule(Pid) -> %% for debug only
    Pid ! {get_schedule, self()},
    receive
	X ->
	    X
    end.


%% Process functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
    spawn(fun() -> loop(#schedule{}) end). %% bad initial value hack, please fix later
		  

loop(Schedule) ->
    receive
	{get_schedule, Caller} -> %% for debug only
	    Caller ! Schedule,
	    loop(Schedule);
	{add_order, OrderFloor, OrderDirection, Caller} ->
	    NewSchedule = add_order_to_schedule(Schedule, #order{floor = OrderFloor, direction = OrderDirection}),
	    Caller ! ok,
	    loop(NewSchedule);
	{remove_order, OrderFloor, OrderDirection, Caller} ->
	    NewSchedule = remove_order_from_schedule(Schedule, #order{floor = OrderFloor, direction = OrderDirection}),
	    Caller ! ok,
	    loop(NewSchedule);
	{get_next_floor, Caller} ->
	    CheapestOrder = get_cheapest_order_from_schedule(Schedule),	    
	    Caller ! {floor, CheapestOrder#order.floor},
	    loop(Schedule);
	{floor_reached, Floor, Caller} ->
	    NewSchedule = change_next_floor_in_schedule(Schedule, Floor),
	    Caller ! ok,
	    loop(NewSchedule);
	{floor_left, Direction, Caller} ->
	    NewSchedule = update_direction_and_increment_floor(Schedule, Direction),
	    Caller ! ok,
	    loop(NewSchedule)
    end.


%% functions for process to call
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

change_next_floor_in_schedule(Schedule, ElevatorNextFloor) ->
    Schedule#schedule{elevator_next_floor = ElevatorNextFloor}.

update_direction_and_increment_floor(Schedule, up) ->
    Schedule#schedule{elevator_next_floor = Schedule#schedule.elevator_next_floor + 1, elevator_direction = up};
update_direction_and_increment_floor(Schedule, down) ->
    Schedule#schedule{elevator_next_floor = Schedule#schedule.elevator_next_floor - 1, elevator_direction = down}.
   

add_order_to_schedule(Schedule, Order) -> % please implement guard for several similar orders, and maybe for unvalid directions ? 
    Schedule#schedule{orders=[Order|Schedule#schedule.orders]}.

remove_order_from_schedule(Schedule, Order) -> % should possibly remove all identical orders
    FilterListCondition = fun(Element) ->
				    if
					Element == Order ->
					    false;
					Element /= Order ->
					    true
				    end
			    end,
    CurrentOrderList = Schedule#schedule.orders,
    NewOrderList = lists:filter(FilterListCondition, CurrentOrderList), 
    Schedule#schedule{orders = NewOrderList}.


	
get_cheapest_order_from_schedule(Schedule) ->
    IncludeCostInListFunction = fun(Order) ->
					{get_cost(Schedule#schedule.elevator_next_floor, 
						  Schedule#schedule.elevator_direction,
						  Order#order.floor,
						  Order#order.direction), Order}
				end,
    CostOrderList = foreach_order(IncludeCostInListFunction, Schedule#schedule.orders),
    {_LeastCost, CheapestOrder} = lists:min(CostOrderList),
    CheapestOrder.




%%% helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	
must_turn(ElevatorNextFloor, up, OrderFloor, up) when OrderFloor >= ElevatorNextFloor ->
    false;
must_turn(ElevatorNextFloor, up, OrderFloor, up) when OrderFloor < ElevatorNextFloor ->
    true;
must_turn(ElevatorNextFloor, down, OrderFloor, down) when OrderFloor =< ElevatorNextFloor ->
    false;
must_turn(ElevatorNextFloor, down, OrderFloor, down) when OrderFloor > ElevatorNextFloor ->
    true;
must_turn(ElevatorNextFloor, up, OrderFloor, command) when OrderFloor >= ElevatorNextFloor ->
    false;
must_turn(ElevatorNextFloor, up, OrderFloor, command) when OrderFloor < ElevatorNextFloor ->
    true;
must_turn(ElevatorNextFloor, down, OrderFloor, command) when OrderFloor =< ElevatorNextFloor ->
    false;
must_turn(ElevatorNextFloor, down, OrderFloor, command) when OrderFloor > ElevatorNextFloor ->
    true;
must_turn(_ElevatorNextFloor, up, _OrderFloor, down) ->
    true;
must_turn(_ElevatorNextFloor, down, _OrderFloor, up) ->
    true;
must_turn(_ElevatorNextFloor, _ElevatorDirection, _OrderFloor, _OrderDirection) ->
    erlang:error(badarg).


get_cost(ElevatorNextFloor, ElevatorDirection, OrderFloor, OrderDirection) ->
    case must_turn(ElevatorNextFloor, ElevatorDirection, OrderFloor, OrderDirection) of
	true ->
	    abs(OrderFloor - ElevatorNextFloor) + ?TURN_COST; 
	false ->
	    abs(OrderFloor - ElevatorNextFloor)
    end.


foreach_order(Function, [LastOrder]) ->
    [Function(LastOrder)];
foreach_order(Function, OrderList) ->
    [Head|Tail] = OrderList,
    [Function(Head)|foreach_order(Function, Tail)].
    

