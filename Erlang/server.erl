-module(server).
-export([start/0]).

start() ->
    {ok, ListenSocket} = gen_tcp:listen(8001, [{active, false}, {packet, 0},{backlog, 50}]),
    [spawn(fun() -> connection_handler(ListenSocket, N) end) || N <- lists:seq(1, 6)].

connection_handler(ListenSocket, N) ->
    {ok, ClientSocket} = gen_tcp:accept(ListenSocket),
    Closer = spawn(fun() -> tcp_closer(6, ClientSocket) end),
    [spawn(fun() -> client_handler(ClientSocket, Closer) end) || _ <- lists:seq(1, 6)],
    connection_handler(ListenSocket, N).

client_handler(ClientSocket, Closer) ->
    case gen_tcp:recv(ClientSocket, 0) of
        {ok, _Data} ->
            ok = gen_tcp:send(ClientSocket, "ok"),
            client_handler(ClientSocket, Closer);
        {error, _Reason} ->
            Closer ! {done} 
    end.

tcp_closer(0, ClientSocket) ->
    ok = gen_tcp:close(ClientSocket); 

tcp_closer(Count, ClientSocket) ->
    receive 
        {done} ->
            tcp_closer(Count - 1, ClientSocket)
    end.

