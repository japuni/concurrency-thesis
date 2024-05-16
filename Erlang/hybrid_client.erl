-module(hybrid_client).
-export([test_java/1, test_erlang/1, test_java/2, test_erlang/2, read_matrix/0]).
read_matrix() ->
    {ok, BinaryContent} = file:read_file("../matrices/100.txt"),
    Content = binary_to_list(BinaryContent),
    I = string:tokens(Content, "\n"),
    X = lists:flatten(I),
    X.


start(Counter, Port, Matrix) ->
    case gen_tcp:connect("localhost", Port, [{active, false}, {mode, list}]) of
        {ok, Socket} ->
            send_message(Counter, Socket, Matrix),
            ok = gen_tcp:close(Socket);
        {error, _} ->
            start(Counter, Port, Matrix)
    end.

recieve_result(Counter, Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Result} ->
            Counter ! {done},
            ok;
        {error, closed} ->
            io:fwrite("Socket closed~n");
        {error, Reason} ->
            io:fwrite("Error, Reason: ~p~n", [Reason])
    end.
send_message(Counter, Socket, Message) ->
    ok = gen_tcp:send(Socket, Message),
    ok = gen_tcp:send(Socket, "EOF"),
    recieve_result(Counter, Socket).

test_java(FileName, Clients) ->
    Durations = [test_java(Clients) || _ <- lists:seq(1, 10)],
    csv_writer:csv_writer(Durations, FileName).

test_java(Clients) ->
    Self = self(),
    Matrix = read_matrix(),
    Counter = spawn(fun() -> counter(Clients , Self) end),
    [spawn(fun() -> start(Counter, 8000, Matrix) end) || _ <- lists:seq(1, Clients)],
    Start = erlang:monotonic_time(nanosecond),
    receive
        {done} ->
            End = erlang:monotonic_time(nanosecond),
            Duration = (End - Start) / 1_000_000_000,
            io:format("Computation time in java: ~f seconds~n", [Duration])
    end,
    Duration.

test_erlang(FileName, Clients) ->
    Durations = [test_erlang(Clients) || lists:seq(1, 10)],
    csv_writer:csv_writer(Durations, FileName).

test_erlang(Clients) ->
    Self = self(),
    Matrix = read_matrix(),
    Counter = spawn(fun() -> counter(Clients, Self) end),
    [spawn(fun() -> start(Counter, 8001, Matrix) end) || _ <- lists:seq(1, Clients)],
    Start = erlang:monotonic_time(nanosecond),
    receive
        {done} ->
            End = erlang:monotonic_time(nanosecond),
            Duration = (End - Start) / 1_000_000_000,
            io:format("Computation time in erlang: ~f seconds~n", [Duration])
    end,
    Duration.

counter(0,Master) ->
    Master ! {done};
counter(N,Master) ->
    receive
        {done} ->
            counter(N - 1, Master);
        {started} ->
            counter(N-1, Master)
    end.
