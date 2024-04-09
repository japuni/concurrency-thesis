-module(server).
-export([start/0]).

start() ->
    {ok, ListenSocket} = gen_tcp:listen(8000, [{active, false}, {packet, 0},{backlog, 50}]),
    [spawn(fun() -> listener(ListenSocket, N) end) || N <- lists:seq(1, 10)].

server_monitor(Pid) ->
    Mref = monitor(process, Pid),
    receive
        {'DOWN', Mref, process, _, _} ->
            io:format("Server went down ~n")
    end.

listener(ListenSocket, N) ->
    {ok, ClientSocket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> client_handler(ClientSocket, N) end),
    listener(ListenSocket, N).

client_handler(ClientSocket, N) ->
    case gen_tcp:recv(ClientSocket, 0) of
        {ok, _Data} ->
            ok = gen_tcp:send(ClientSocket, "ok"),
            client_handler(ClientSocket, N);
        {error, Reason} ->
            ok = gen_tcp:close(ClientSocket)
    end.
