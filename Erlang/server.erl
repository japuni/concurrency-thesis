-module(server).
-export([start/0, handle_client/1]).

start() ->
    {ok, ListenSocket} = gen_tcp:listen(8000, [{active, false}, {packet, 0}]),
    Pid = spawn(fun() -> accept_clients(ListenSocket)end),
    spawn(fun() -> server_monitor(Pid)end).

server_monitor(Pid) ->
    Mref = monitor(process, Pid),
    receive
        {'DOWN', Mref, process, _, _} ->
            io:format("Server went down ~n")
    end.

accept_clients(ListenSocket) ->
    {ok, ClientSocket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> handle_client(ClientSocket) end),
    accept_clients(ListenSocket).

handle_client(ClientSocket) ->
    case gen_tcp:recv(ClientSocket, 0) of
        {ok, _Data} ->
            ok = gen_tcp:send(ClientSocket, "ok"),
            ok = gen_tcp:close(ClientSocket);
        {error, Reason} ->
            io:format("Failed to receive data from client: ~p~n", [Reason]),
            ok = gen_tcp:close(ClientSocket)
    end.
