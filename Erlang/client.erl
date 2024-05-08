-module(client).
-export([test_java/2, test_erlang/2, test_hybrid_java/2, test_hybrid_erlang/2]).
start(Id, Counter, Port, Messages) ->
    case gen_tcp:connect("localhost", Port, [{active, false}, {mode, list}]) of
        {ok, Socket} ->
            loop(Id, Socket, Counter, Messages, {1, 2}),
            ok = gen_tcp:close(Socket);
        {error, _} ->
            start(Id, Counter, Port, Messages)
    end.

loop(Id, _Socket, _Counter, 0, _) ->
    io:fwrite("Client:~p is done~n", [Id]);

loop(Id, Socket, Counter, Messages, {X, Y}) ->
    ok = gen_tcp:send(Socket, io_lib:format("~p+~p~n", [X, Y])),
    Result = hd(io_lib:format("~p", [X + Y])),
    io:format("Result = ~p~n", [Result]),
    case gen_tcp:recv(Socket, 0) of
        {ok, Result} ->
            Counter ! {done},
            loop(Id, Socket, Counter, Messages-1, {X+2, Y+1});
        {ok, Data} ->
            io:format("WRONG REPLY, EXPECTED: ~p, GOT: ~p~n", [Result, Data]),
            loop(Id, Socket, Counter, Messages-1, {X+2, Y+1});
        {error, closed} ->
            io:fwrite("Client : ~p Socket closed~n", [Id]);
        {error, Reason} ->
            io:fwrite("Client ~p: Error, Reason: ~p~n", [Id, Reason])
    end.

test_java(Clients, Messages) ->
    Self = self(),
    TotalMessages = Messages * Clients,
    Counter = spawn(fun() -> counter(TotalMessages , Self) end),
    Start = erlang:monotonic_time(nanosecond),
    [spawn(fun() -> start(Id, Counter, 8000, Messages) end) || Id <- lists:seq(1, Clients)],
    receive
        {done} ->
      End = erlang:monotonic_time(nanosecond),
      Duration = (End - Start) / 1_000_000_000,
      io:format("Computation time in java: ~f seconds~n", [Duration]),
      io:format("Throughput in java: ~f clients served per second", [(TotalMessages / Duration)])
    end,
    ok.

test_erlang(Clients, Messages) ->
    Self = self(),
    TotalMessages = Messages * Clients,
    Counter = spawn(fun() -> counter(TotalMessages, Self) end),
    Start = erlang:monotonic_time(nanosecond),
    [spawn(fun() -> start(Id, Counter, 8001, Messages) end) || Id <- lists:seq(1, Clients)],
    receive
        {done} ->
      End = erlang:monotonic_time(nanosecond),
      Duration = (End - Start) / 1_000_000_000,
      io:format("Computation time in erlang: ~f seconds~n", [Duration]),
      io:format("Throughput in erlang: ~f clients served per second", [(TotalMessages / Duration)])
    end,
    ok.

start_hybrid(Id, Counter, Port, Messages) ->
    case gen_tcp:connect("localhost", Port, [{active, false}, {mode, list}]) of
        {ok, Socket} ->
            hybrid_loop(Id, Socket, Counter, Messages, 
                        {generate_matrix(5), generate_matrix(5)}),
            ok = gen_tcp:close(Socket);
        {error, _} ->
            start_hybrid(Id, Counter, Port, Messages)
    end.

hybrid_loop(Id, _Socket, _Counter, 0, _) ->
    io:fwrite("Client~p is done~n", [Id]);

hybrid_loop(Id, Socket, Counter, Messages, {MatrixA, MatrixB}) ->
    io:format(" ~p~n", [MatrixA]),
    ok = gen_tcp:send(Socket, [ 5 | MatrixA]),
    case gen_tcp:recv(Socket, 0) of 
        {ok, Result} ->
            Counter ! {done},
            loop(Id, Socket, Counter, Messages -1, {0, MatrixB});
        {error, closed} ->
            io:fwrite("Client : ~p Socket closed~n", [Id]);
        {error, Reason} ->
            io:fwrite("Client ~p: Error, Reason: ~p~n", [Id, Reason])
    end.



test_hybrid_java(Clients, Messages) ->
    Self = self(),
    TotalMessages = Messages * Clients,
    Counter = spawn(fun() -> counter(TotalMessages , Self) end),
    Start = erlang:monotonic_time(nanosecond),
    [spawn(fun() -> start_hybrid(Id, Counter, 8000, Messages) end) || Id <- lists:seq(1, Clients)],
    receive
        {done} ->
            End = erlang:monotonic_time(nanosecond),
            Duration = (End - Start) / 1_000_000_000,
            io:format("Computation time in java: ~f seconds~n", [Duration]),
            io:format("Throughput in java: ~f clients served per second", [(TotalMessages / Duration)])
    end,
    ok.

test_hybrid_erlang(Clients, Messages) ->
    Self = self(),
    TotalMessages = Messages * Clients,
    Counter = spawn(fun() -> counter(TotalMessages, Self) end),
    Start = erlang:monotonic_time(nanosecond),
    [spawn(fun() -> start_hybrid(Id, Counter, 8001, Messages) end) || Id <- lists:seq(1, Clients)],
    receive
        {done} ->
            End = erlang:monotonic_time(nanosecond),
            Duration = (End - Start) / 1_000_000_000,
            io:format("Computation time in erlang: ~f seconds~n", [Duration]),
            io:format("Throughput in erlang: ~f clients served per second", [(TotalMessages / Duration)])
    end,
    ok.

counter(0,Master) ->
    Master ! {done};
counter(N,Master) ->
    receive
        {done} ->
            counter(N - 1, Master);
        {started} ->
            counter(N-1, Master)
    end.
generate_matrix(Size) ->
    [[rand:uniform(100) || _ <- lists:seq(1, Size)] || _ <- lists:seq(1, Size)].
