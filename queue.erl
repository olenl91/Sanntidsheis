-module(queue).
-export([init/0, add/2, get/1, remove/2, get_next_order/2]).


init() ->
    ets:new(queue, [bag, named_table]).

add(Floor, Direction) ->
    ets:insert(queue, {Floor, Direction}).

get(Floor) ->
    ets:lookup(queue, Floor).

remove(Floor, Direction) ->
    ets:delete_object(queue, {Floor, Direction}).


get_next_order(Floor, Direction) ->
    ets:foldl(fun(E, Last) -> E end, {0, lol}, queue).
		       

    
