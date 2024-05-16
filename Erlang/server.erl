-module(server).
-export([start_paralell/1, start_sequential/0]).

start_sequential() ->
    {ok, ListenSocket} = gen_tcp:listen(8001, [{active, false}, {packet, 0},{backlog, 100}]),
    spawn(fun() -> sequential_connection_handler(ListenSocket) end).

start_paralell(AmountOfWorkers) ->
    {ok, ListenSocket} = gen_tcp:listen(8001, [{active, false}, {packet, 0},{backlog, 100}]),
    WorkPool = spawn(fun() -> workpool([], []) end),
    Workers = [spawn(fun() -> paralell_client_handler(WorkPool, Id) end) || Id <- lists:seq(1, AmountOfWorkers)],
    [WorkPool ! {done, Worker} || Worker <- Workers],
    spawn(fun() -> paralell_connection_handler(ListenSocket, WorkPool) end).

sequential_connection_handler(ListenSocket) ->
    {ok, ClientSocket} = gen_tcp:accept(ListenSocket),
    client_handler(ClientSocket, 0, 0),
    sequential_connection_handler(ListenSocket).

client_handler(ClientSocket, Id, Count) ->
    case gen_tcp:recv(ClientSocket, 0) of
        {ok, Data} ->
            {X, Rest} = string:to_integer(Data),
            {Y, _} = string:to_integer(Rest),
            Result = X + Y,
            io:format("Result = ~p Kommit hit ~p loop:~p~n", [Result, Id, Count]),
            ok = gen_tcp:send(ClientSocket, io_lib:format("~p", [Result])),
            client_handler(ClientSocket, Id, Count+1);
        {error, _Reason} ->
            done
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
           workpool([Worker | AvailableWorkers], []);
        Annat -> io:format("något annat ~p~n", [Annat]) 
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

paralell_client_handler(WorkPool, Id) ->
    receive 
        {client, Client} ->
            ClientSocket = Client
    end,
    client_handler(ClientSocket, Id, 1),
    WorkPool ! {done, self()},
    paralell_client_handler(WorkPool, Id).
