-module(scheduler).
-compile(export_all).



% make bidding on order abstraction consistent


%% Module interface
%%%%%%%%%%%%%%%%%%%

%CostCalculationFunctin(Floor, Direction)
request_order(SchedulerPID, CostCalculationFunction) -> % this is probably a bad name
    SchedulerPID ! {order_request, self()},
    {Floor, Direction, Cost} = receive % get some weird warning here
				   {order_offer, Floor, Direction} ->
				       Cost = CostCalculationFunction(Floor, Direction),
				       SchedulerPID ! {cost, self()},
				       {Floor, Direction, Cost}
			       end,
    
    SchedulerPID ! {order_bid, Floor, Direction, Cost, self()}, 
    
    Status = receive
		 {order_status, Floor, Direction, ReceivedStatus} ->
		     ReceivedStatus
	     after 
		 2000 -> % this needs to be a bit less random
		     timedout
	     end,
    
    {Floor, Direction, Status}.

    


%% Call Backs
%%%%%%%%%%%%%%%%%%%


%% Process functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
    spawn(fun() -> loop() end).

loop() ->
    schedule_order(2, down),
    timer:sleep(10000),
    loop().



%% Function for modules to use
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

schedule_order(Floor, Direction) ->
    MemberList = connect_to_members(),
    case MemberList of
	[] ->
	    no_members; %this is somewhat hacky
	_ ->
	    send_order_to_all_members(MemberList, Floor, Direction),
	    {_LeastCost, WinningMember} = receive_bids_from_members(MemberList),
	    send_status_to_members(MemberList, WinningMember, Floor, Direction),
	    ok
    end.

    

%% Helper functions for schedule_order
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

connect_to_members() -> connect_to_members([]).
connect_to_members(MemberList) ->
    receive
	{order_request, Member} ->
	    connect_to_members([Member|MemberList])
    after 
	0 ->
	    MemberList
    end.


send_order_to_all_members([LastMember], Floor, Direction) ->
    LastMember ! {order_offer, Floor, Direction};
send_order_to_all_members(MemberList, Floor, Direction) ->
    [HeadMember|TailMembers] = MemberList,
    HeadMember ! {order_offer, Floor, Direction},
    send_order_to_all_members(TailMembers, Floor, Direction).


receive_bids_from_members(MemberList) -> %This is a pretty shit name in regards to what it returns
    receive_bids_from_members(MemberList, [], []).
receive_bids_from_members(MemberList, MembersCommited, OfferList) ->
    receive 
	{order_bid, _Floor, _Direction, Cost, Member} -> % is a bit fragile since it's no checking that order corresponds to whatever it's bidding on
	    receive_bids_from_members(MemberList, [Member|MembersCommited], [{Cost, Member}|OfferList])
    after
	500 -> %pretty random value, do this thing better
	    {_LeastCost, _WinningMember} = lists:min(OfferList)    
    end.
    
	


send_status_to_members(MemberList, WinningMember, Floor, Direction) ->
    LosingMembers = lists:delete(WinningMember, MemberList),
    
    SendLostToLosingMembers = fun(LosingMember) ->
				      LosingMember ! {order_status, Floor, Direction, lost}
			      end,
    
    WinningMember ! {order_status, Floor, Direction, won},
    lists:foreach(SendLostToLosingMembers, LosingMembers),
    ok.
