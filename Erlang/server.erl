-module(server).
-export([start/0]).

start() ->
    {ok, ListenSocket} = gen_tcp:listen(8001, [{active, false}, {packet, 0},{backlog, 50}]),
    [spawn(fun() -> connection_handler(ListenSocket) end) || _ <- lists:seq(1, 6)].

connection_handler(ListenSocket) ->
    {ok, ClientSocket} = gen_tcp:accept(ListenSocket),
    Closer = spawn(fun() -> tcp_closer(6, ClientSocket) end),
    [spawn(fun() -> client_handler(ClientSocket, Closer) end) || _ <- lists:seq(1, 6)],
    connection_handler(ListenSocket).

client_handler(ClientSocket, Closer) ->
    case gen_tcp:recv(ClientSocket, 0) of
        {ok, Data} ->
            {X, Rest} = string:to_integer(Data),
            {Y, _} = string:to_integer(Rest),
            Result = X + Y,
            ok = gen_tcp:send(ClientSocket, io_lib:format("~p", [Result])),
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

