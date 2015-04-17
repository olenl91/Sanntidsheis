-module(connection_manager).
-compile(export_all).

-define(SEND_PORT, 5678).
-define(RECV_PORT, 5677).
-define(COOKIE, "erlang"). 
-define(SEEK_PERIOD, 5000).

listen_for_connections() ->
    {ok, RecvSocket} = gen_udp:open(?RECV_PORT, [list, {active,false}]),
    {ok, {Adress, ?SEND_PORT, ?COOKIE}} = gen_udp:recv(RecvSocket, 0), % kan kresje dersom Cookie er feil
    gen_udp:close(RecvSocket),
    Adress.


is_in_cluster(Node) ->
    NodeList = [node()|nodes()],
    lists:member(Node, NodeList).

connect_to_node(Node) ->
    net_adm:ping(Node). %might be not very intuitive return value, should maybe crash if not possible


broadcast_loop() ->
    {ok, Socket} = gen_udp:open(?SEND_PORT, [list, {active,true}, {broadcast, true}]),
    broadcast_loop(Socket).
broadcast_loop(SendSocket) ->
    ok = gen_udp:send(SendSocket, {255,255,255,255}, ?RECV_PORT, ?COOKIE),
    timer:sleep(?SEEK_PERIOD),
    broadcast_loop(SendSocket).

    