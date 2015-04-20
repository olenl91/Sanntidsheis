-module(order_db).
-compile(export_all).

-record(order, {floor, direction}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Network interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

install() ->
    rpc:multicall(order_db, create_table, []).

add_order(Floor, Direction) ->
    rpc:multicall(order_db, insert_order, [Floor, Direction]).

remove_order(Floor, Direction) ->
    rpc:multicall(order_db, delete_order, [Floor, Direction]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Syncing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sync_ets([NodeList]) ->
    NodeList = lists:append([[node()],nodes()]),
    sync(NodeList).

sync([]) -> ok;
sync([NodeToSync|NextNodes]) ->
    do_sync(NodeToSync),
    sync(NextNodes).

do_sync([]) -> ok;
do_sync(NodeToSync) ->
    register(sync, self()),
        true ->
            OrderList = ets:tab2list(orders);
        false ->
            rpc:call(NodeToSync, order_db, send_Data,[node()]),
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

send_Data(ToNode) ->
    {sync, ToNode} ! {[node()], ets:tab2list(orders)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DB interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_table() ->
    ets:new(orders, [named_table, {keypos, #order.floor}, bag]).

insert_order(Floor, Direction) ->
    case is_order(Floor, Direction) of
        true ->
            already_exists;
        false ->
            ets:insert(orders, #order{floor = Floor, direction = Direction})
    end.

delete_order(Floor, Direction) ->
    ets:delete(orders, #order{floor = Floor, direction = Direction}).

is_order(Floor, Direction) ->
    case ets:match(orders, #order{floor = Floor, direction = Direction}) of
        [] ->
            false;
        _ ->
            true
        end.
