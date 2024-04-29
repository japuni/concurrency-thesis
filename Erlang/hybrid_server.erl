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
    sequential_client_handler(ClientSocket, 0, 0),
    sequential_connection_handler(ListenSocket).

parallel_connection_handler(ListenSocket) ->
    {ok, ClientSocket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> client_handler(ClientSocket, 0,0) end),
    parallel_connection_handler(ListenSocket).

client_handler(ClientSocket, MatrixA, MatrixB) when is_list(MatrixB) ->
    Result = matrix_multiplier:multiply_parallel(MatrixA, MatrixB, 12),
    gen_tcp:send(ClientSocket, Result);

client_handler(ClientSocket, MatrixA, MatrixB) ->
    case gen_tcp:recv(ClientSocket, 0) of
        {ok, Data} when is_integer(MatrixA)->
            client_handler(ClientSocket, Data, MatrixB);
        {ok, Data} when is_integer(MatrixB)->
            client_handler(ClientSocket, MatrixA, Data);
        {error, _Reason} ->
            done

    end.

sequential_client_handler(ClientSocket, MatrixA, MatrixB) when is_list(MatrixB) ->
    Result = matrix_multiplier:multiply(MatrixA, MatrixB),
    gen_tcp:send(ClientSocket, Result);

sequential_client_handler(ClientSocket, MatrixA, MatrixB) ->
    case gen_tcp:recv(ClientSocket, 0) of
        {ok, Data} when is_integer(MatrixA)->
            client_handler(ClientSocket, Data, MatrixB);
        {ok, Data} when is_integer(MatrixB)->
            client_handler(ClientSocket, MatrixA, Data);
        {error, _Reason} ->
            done

    end.
