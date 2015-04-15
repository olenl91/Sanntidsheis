-module(mnesia_interface).
-compile(export_all).

-record(orders, {order_number, button_type, floor}). % might need something about origin cause of command orders


install(Nodes) ->
	mnesia:create_schema(Nodes),
        rpc:multicall(Nodes, application, start, [mnesia]),
        mnesia:create_table(orders,
                        [{attributes, record_info(fields, orders)},
                         {index, [#orders.floor]},
                         {ram_copies, Nodes}]).
