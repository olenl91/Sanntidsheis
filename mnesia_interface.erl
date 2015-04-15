-module(mnesia_interface).
-compile(export_all).

-record(order, {floor, direction}). % might need something about origin to handle internal orders


install(Nodes) ->
    mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [mnesia]),
    mnesia:create_table(orders, [
				 {record_name, order},
				 {attributes, record_info(fields, order)},
				 {ram_copies, Nodes}
				]).
