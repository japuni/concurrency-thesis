-module(server).
-export([start/0]).

start() ->
    {ok, ListenSocket} = gen_tcp:listen(8001, [{active, false}, {packet, 0},{backlog, 50}]),
    [spawn(fun() -> listener(ListenSocket, N, 0) end) || N <- lists:seq(1, 6)].


listener(ListenSocket, N, Count) when Count rem 10 =:= 0 ->
    io:format("Listener nr:~p has served ~p connections ~n", [N, Count]),
    {ok, ClientSocket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> client_handler(ClientSocket) end),
    listener(ListenSocket, N, Count + 1);

listener(ListenSocket, N, Count) ->
    {ok, ClientSocket} = gen_tcp:accept(ListenSocket),
    [spawn(fun() -> client_handler(ClientSocket) end) || _ <- lists:seq(1, 6)],
    listener(ListenSocket, N, Count + 1).

client_handler(ClientSocket) ->
    case gen_tcp:recv(ClientSocket, 0) of
        {ok, _Data} ->
            ok = gen_tcp:send(ClientSocket, "ok"),
            client_handler(ClientSocket);
        {error, _Reason} ->
            ok = gen_tcp:close(ClientSocket)
    end.
