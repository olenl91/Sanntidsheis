-module(order_db).
-compile(export_all).
-include_lib ("mnesia/src/mnesia.hrl"). %% Include to get the mnesia types, used in create table
-record(order, {floor, direction}). % might need something about origin to handle internal orders

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DB interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
    case is_order(Floor, Direction) of
	true ->
	    already_exists;
	false ->

	    AddOrderTransaction = fun() ->
					  mnesia:write(orders, #order{direction=Direction, floor=Floor}, write)
				  end,
	    ok = mnesia:activity(transaction, AddOrderTransaction)
    end.


remove_order(Floor, Direction) ->
    RemoveOrderTransaction = fun() ->
				     mnesia:delete_object(orders, #order{floor=Floor, direction=Direction}, write)
			     end,
    mnesia:activity(transaction, RemoveOrderTransaction).

is_order(Floor, Direction) ->
    OrderList = get_order_list(),
    lists:member(#order{floor = Floor, direction = Direction}, OrderList).

add_new_node_to_cluster(Node) ->
	net_adm:ping(Node),
    	rpc:call(Node, application, start, [mnesia]),
	mnesia:change_config(extra_db_nodes, [Node]),
	mnesia:change_table_copy_type(schema, Node, ram_copies),
	mnesia:add_table_copy(orders, Node, ram_copies).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% functions used to reconnect a node after a netsplit
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sync_mnesia() ->
	rpc:multicall(mnesia, 'dump_to_textfile', ['BackupMnesia']),
	reinit_mnesia_cluster(),
	rpc:multicall(mnesia, 'load_textfile', ['BackupMnesia']).
	
reinit_mnesia_cluster() -> 
	rpc:multicall(mnesia, stop, []),
	mnesia:delete_schema(lists:append([[node()], nodes()])),
	install(lists:append([[node()], nodes()])).

%%%%%%%%%%%%%%%%%%%%%%
%% helper functions
%%%%%%%%%%%%%%%%%%%%%%

get_order_list() ->
    GetAllOrdersTransaction = fun() ->
				      mnesia:match_object(orders, #order{floor='_', direction='_'}, read)
			      end,
    mnesia:activity(transaction, GetAllOrdersTransaction).








