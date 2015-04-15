-module(mnesia_interface). % should maybe be called something as order_storage or similar
-compile(export_all).

-record(order, {floor, direction}). % might need something about origin to handle internal orders


install(Nodes) ->
    mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [mnesia]),
    mnesia:create_table(orders, [
				 {record_name, order},
				 {attributes, record_info(fields, order)},
				 {ram_copies, Nodes},
				 {type, bag}
				]).


add_order(Floor, Direction) ->
    AddOrderTransaction = fun() ->
				  mnesia:write(orders, #order{direction=Direction, floor=Floor}, write)
			  end,
    mnesia:activity(transaction, AddOrderTransaction).

remove_order(Floor, Direction) ->
    RemoveOrderTransaction = fun() ->
				     mnesia:delete_object(orders, #order{floor=Floor, direction=Direction}, write)
			     end,
    mnesia:activity(transaction, RemoveOrderTransaction).

is_order(Floor, Direction) ->
    OrderList = get_order_list(),
    lists:member(#order{floor = Floor, direction = Direction}, OrderList).



%% helper functions
%%%%%%%%%%%%%%%%%%%    

get_order_list() ->
    GetAllOrdersTransaction = fun() ->
				      mnesia:match_object(orders, #order{floor='_', direction='_'}, read)
			      end,
    mnesia:activity(transaction, GetAllOrdersTransaction).
