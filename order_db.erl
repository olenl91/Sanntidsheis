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

%% sync_ets() %%% to do

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







