-module(hybrid_server).
-export([start_parallel/0, start_sequential/0, parse/1]).

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
    Result = matrix_multiplier:multiply(MatrixA, MatrixB),
    ok = gen_tcp:send(ClientSocket, io_lib:format("~p", [Result])).

client_handler(ClientSocket) ->
    case gen_tcp:recv(ClientSocket, 0) of
        {ok, Data} -> 
            Matrix = parse(Data),
            result(ClientSocket, Matrix, Matrix);
            % result(ClientSocket, A, string:replace(B, "\n", ""));
            
        {error, Reason} ->
            io:format("Error: ~p ~n", [Reason]),
            ok
    end.
parse(String) ->
    {ok, Tokens, _} = erl_scan:string(String),
    Dot = case lists:last(Tokens) of
              {dot, _} -> [];
              _ -> [{dot, 1}]
          end,
    {ok, Result} = erl_parse:parse_term(Tokens ++ Dot),
    Result.
