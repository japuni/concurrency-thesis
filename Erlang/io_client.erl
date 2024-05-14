-module(io_client).
-export([test_java/2, test_erlang/2, test_java/3, test_erlang/3]).

start(Counter, Port, Messages) ->
    case gen_tcp:connect("localhost", Port, [{active, false}, {mode, list}]) of
        {ok, Socket} ->
            loop(Socket, Counter, Messages, {1, 2}),
            ok = gen_tcp:close(Socket);
        {error, _} ->
            start(Counter, Port, Messages)
    end.

loop(_Socket, _Counter, 0, _) ->
    ok;
loop(Socket, Counter, Messages, {X, Y}) ->
    ok = gen_tcp:send(Socket, io_lib:format("~p+~p~n", [X, Y])),
    Result = hd(io_lib:format("~p", [X + Y])),
    io:format("Result = ~p~n", [Result]),
    case gen_tcp:recv(Socket, 0) of
        {ok, Result} ->
            Counter ! {done},
            loop(Socket, Counter, Messages-1, {X+2, Y+1});
        {ok, Data} ->
            io:format("WRONG REPLY, EXPECTED: ~p, GOT: ~p~n", [Result, Data]),
            loop(Socket, Counter, Messages-1, {X+2, Y+1});
        {error, closed} ->
            io:fwrite("Socket closed~n");
        {error, Reason} ->
            io:fwrite("Error, Reason: ~p~n", [Reason])
    end.

test_java(FileName, Clients, Messages) ->
    Durations = [test_java(Clients, Messages) || lists:seq(1, 10)],
    csv_writer:csv_writer(Durations, FileName, 0).

test_java(Clients, Messages) ->
    Self = self(),
    TotalMessages = Messages * Clients,
    Counter = spawn(fun() -> counter(TotalMessages , Self) end),
    Start = erlang:monotonic_time(nanosecond),
    [spawn(fun() -> start(Counter, 8000, Messages) end) || _ <- lists:seq(1, Clients)],
    receive
        {done} ->
      End = erlang:monotonic_time(nanosecond),
      Duration = (End - Start) / 1_000_000_000,
      io:format("Computation time in java: ~f seconds~n", [Duration]),
      io:format("Throughput in java: ~f clients served per second", [(TotalMessages / Duration)])
    end,
    Duration.

test_erlang(FileName, Clients, Messages) ->
    Durations = [test_java(Clients, Messages) || lists:seq(1, 10)],
    csv_writer:csv_writer(Durations, FileName, 0).

test_erlang(Clients, Messages) ->
    Self = self(),
    TotalMessages = Messages * Clients,
    Counter = spawn(fun() -> counter(TotalMessages, Self) end),
    Start = erlang:monotonic_time(nanosecond),
    [spawn(fun() -> start(Counter, 8001, Messages) end) || _ <- lists:seq(1, Clients)],
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
