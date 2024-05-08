-module(hybrid_server).
-export([start_parallel/0, start_sequential/0]).

start_sequential() ->
    {ok, ListenSocket} = gen_tcp:listen(8001, [{active, false}, {packet, 0},{backlog, 100}]),
    spawn(fun() -> sequential_connection_handler(ListenSocket) end).
start_parallel() ->
    {ok, ListenSocket} = gen_tcp:listen(8001, [{active, false}, {packet, 0},{backlog, 100}]),
    spawn(fun() -> parallel_connection_handler(ListenSocket) end).

sequential_connection_handler(ListenSocket) ->
    {ok, ClientSocket} = gen_tcp:accept(ListenSocket),
    client_handler(ClientSocket),
    sequential_connection_handler(ListenSocket).

parallel_connection_handler(ListenSocket) ->
    {ok, ClientSocket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> client_handler(ClientSocket) end),
    parallel_connection_handler(ListenSocket).

result(ClientSocket, MatrixA, MatrixB) ->
    io:format("trying to multiply~n"),
    matrix_multiplier:multiply(MatrixA, MatrixB),
    io:format("somehting more happened ~n"),
    gen_tcp:send(ClientSocket, ok),
    client_handler(ClientSocket).

client_handler(ClientSocket) ->
    io:format("Something happened~n"),
    case gen_tcp:recv(ClientSocket, 0) of
        {ok, MatrixA} -> 
            io:format("MatrixA ~p~n", [MatrixA]),
            io:format("size : ~p~n", [hd(MatrixA)]);
            % client_handler(ClientSocket, MatrixA);
        {error, Reason} ->
            io:format("Error: ~p ~n", [Reason]),
            ok
    end.
