-module(server).
-export([start_paralell/0, start_sequential/0]).

start_sequential() ->
    {ok, ListenSocket} = gen_tcp:listen(8001, [{active, false}, {packet, 0},{backlog, 100}]),
    spawn(fun() -> sequential_connection_handler(ListenSocket) end).
start_paralell() ->
    {ok, ListenSocket} = gen_tcp:listen(8001, [{active, false}, {packet, 0},{backlog, 100}]),
    spawn(fun() -> paralell_connection_handler(ListenSocket) end).

sequential_connection_handler(ListenSocket) ->
    {ok, ClientSocket} = gen_tcp:accept(ListenSocket),
    client_handler(ClientSocket),
    sequential_connection_handler(ListenSocket).

paralell_connection_handler(ListenSocket) ->
    {ok, ClientSocket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> client_handler(ClientSocket) end),
    paralell_connection_handler(ListenSocket).

client_handler(ClientSocket) ->
    case gen_tcp:recv(ClientSocket, 0) of
        {ok, Data} ->
            {X, Rest} = string:to_integer(Data),
            {Y, _} = string:to_integer(Rest),
            Result = X + Y,
            ok = gen_tcp:send(ClientSocket, io_lib:format("~p", [Result])),
            client_handler(ClientSocket);
        {error, _Reason} ->
            done

    end.


