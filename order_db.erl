-module(order_db).
-compile(export_all).

-record(order, {floor, direction}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Network interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

install() ->
    do_install(lists:append([[node()],nodes()])).

do_install([]) -> ok;
do_install([Node|NextNodes]) ->
    case Node == node() of
        true ->
            create_table();
        false ->
            %spawn process on Node
            send_table_order(Node),
            %receive on Node..
            %and create_table on Node
        end,
    do_install(NextNodes).

send_table_order(Node) ->
    {create_table, Node} ! {Node, do_install}.

%%%% Do similar to replace rpc:multicall for add_order and remove_order

add_order(Floor, Direction) ->
    rpc:multicall(order_db, insert_order, [Floor, Direction]).

remove_order(Floor, Direction) ->
    rpc:multicall(order_db, delete_order, [Floor, Direction]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Syncing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sync_ets() ->
    sync(lists:append([[node()],nodes()])).

sync([]) -> ok;
sync([NodeToSync|NextNodes]) ->
    do_sync(NodeToSync),
    sync(NextNodes).

do_sync([]) -> ok;
do_sync(NodeToSync) ->
    register(sync, self()),
    case NodeToSync == node() of
        true ->
            OrderList = ets:tab2list(orders);
        false ->
            rpc:call(NodeToSync, order_db, send_order_data,[node()]),
            receive
                    {NodeToSync, OrderList} ->
                        ok
            end
        end,
        sync_orders(OrderList).

sync_orders([OrderToSync|NextOrders]) ->
    do_sync_orders(OrderToSync),
    sync_orders(NextOrders).

do_sync_orders(#order{floor = Floor, direction = Direction}) ->
    add_order(Floor, Direction).

send_order_data(ToNode) ->
    {sync, ToNode} ! {[node()], ets:tab2list(orders)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DB interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_table() ->
    ets:new(orders, [public, named_table, {keypos, #order.floor}, bag]).

insert_order(Floor, Direction) ->
    case is_order(Floor, Direction) of
        true ->
            already_exists;
        false ->
            ets:insert(orders, #order{floor = Floor, direction = Direction})
    end.

delete_order(Floor, Direction) ->
    ets:delete(orders, #order{floor = Floor, direction = Direction}).  %Needs to be fixed, somehow

is_order(Floor, Direction) ->
    case ets:match(orders, #order{floor = Floor, direction = Direction}) of
        [] ->
            false;
        _ ->
            true
        end.
