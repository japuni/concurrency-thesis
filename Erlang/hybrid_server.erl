-module(hybrid_server).
-export([start_paralell/1, start_sequential/0, parse/1]).

start_sequential() ->
    {ok, ListenSocket} = gen_tcp:listen(8001, [{active, false}, {packet, 0},{backlog, 100}]),
    spawn(fun() -> sequential_connection_handler(ListenSocket, 1) end).

start_paralell(AmountOfWorkers) ->
    {ok, ListenSocket} = gen_tcp:listen(8001, [{active, false}, {packet, 0},{backlog, 100}]),
    WorkPool = spawn(fun() -> workpool([], []) end),
    Workers = [spawn(fun() -> paralell_client_handler(WorkPool) end) || _ <- lists:seq(1, AmountOfWorkers)],
    [WorkPool ! {done, Worker} || Worker <- Workers],
    spawn(fun() -> paralell_connection_handler(ListenSocket, WorkPool) end).

sequential_connection_handler(ListenSocket, Count) ->
    {ok, ClientSocket} = gen_tcp:accept(ListenSocket),
    client_handler(ClientSocket, []),
    io:format("Here: ~p~n", [Count]),
    sequential_connection_handler(ListenSocket, Count + 1).

client_handler(ClientSocket, Acc) ->
    case gen_tcp:recv(ClientSocket, 0) of
        {ok, "EOF"} ->
            io:format("hello~n"),
            Matrix = parse(lists:flatten(lists:reverse(Acc))),
            Result = matrix_multiplier:multiply(Matrix, Matrix),
            ok = gen_tcp:send(ClientSocket, io_lib:format("~p", [Result]));
        {ok, Data} ->
            case is_end_of_file(Data, []) of
                {true, EndOfMatrix} ->
                    Matrix = parse(lists:flatten(lists:reverse([EndOfMatrix | Acc]))),
                    Result = matrix_multiplier:multiply(Matrix, Matrix),
                    ok = gen_tcp:send(ClientSocket, io_lib:format("~p", [Result]));
                {false, PartOfMatrix} ->
                    client_handler(ClientSocket, [PartOfMatrix | Acc])
            end;
        {error, Reason} ->
            io:format("Error: ~p ~n", [Reason]),
            ok
    end.

paralell_connection_handler(ListenSocket, WorkPool) ->
    {ok, ClientSocket} = gen_tcp:accept(ListenSocket),
    WorkPool ! {client, ClientSocket},
    paralell_connection_handler(ListenSocket, WorkPool).

workpool(AvailableWorkers, []) ->
    receive
        {client, ClientSocket} ->
            workpool(AvailableWorkers, [ClientSocket]);
        {done, Worker} ->
           workpool([Worker | AvailableWorkers], [])
    end;

workpool([], Clients) ->
    receive
        {client, ClientSocket} ->
            workpool([], [ClientSocket | Clients]);
        {done, Worker} ->
           workpool([Worker], Clients)
    end;

workpool([Worker | Rest], [Client | Clients]) ->
    Worker ! {client, Client},
    workpool(Rest, Clients).

paralell_client_handler(WorkPool) ->
    receive 
        {client, Client} ->
            ClientSocket = Client
    end,
    client_handler(ClientSocket, []),
    WorkPool ! {done, self()},
    paralell_client_handler(WorkPool).

parse(String) ->
    {ok, Tokens, _} = erl_scan:string(String),
    Dot = case lists:last(Tokens) of
              {dot, _} -> [];
              _ -> [{dot, 1}]
          end,
    {ok, Result} = erl_parse:parse_term(Tokens ++ Dot),
    Result.

is_end_of_file([], Acc) ->
    {false, lists:reverse(Acc)};
is_end_of_file([Char | Rest], Acc) ->
    case Char of 
        $E -> 
            {true, lists:reverse(Acc)};
        _ ->
            is_end_of_file(Rest, [Char | Acc])
    end.


