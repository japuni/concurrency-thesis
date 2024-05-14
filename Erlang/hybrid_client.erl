-module(hybrid_client).
-export([test_java/1, test_erlang/1, test_java/2, test_erlang/2, read_matrix/0]).
read_matrix() ->
    {ok, BinaryContent} = file:read_file("100.txt"),
    Content = binary_to_list(BinaryContent),
    I = string:tokens(Content, "\n"),
    lists:flatten(I).

start(Counter, Port) ->
    Message = read_matrix(),
    case gen_tcp:connect("localhost", Port, [{active, false}, {mode, list}]) of
        {ok, Socket} ->
            send_message(Counter, Socket, Message),
            ok = gen_tcp:close(Socket);
        {error, _} ->
            start(Counter, Port)
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
  recieve_result(Counter, Socket).

test_java(FileName, Clients) ->
    Durations = [test_java(Clients) || lists:seq(1, 10)],
    csv_writer:csv_writer(Durations, FileName, 0).

test_java(Clients) ->
    Self = self(),
    Counter = spawn(fun() -> counter(Clients , Self) end),
    Start = erlang:monotonic_time(nanosecond),
    [spawn(fun() -> start(Counter, 8000) end) || _ <- lists:seq(1, Clients)],
    receive
        {done} ->
            End = erlang:monotonic_time(nanosecond),
            Duration = (End - Start) / 1_000_000_000,
            io:format("Computation time in java: ~f seconds~n", [Duration])
    end,
    ok.

test_erlang(FileName, Clients) ->
    Durations = [test_java(Clients) || lists:seq(1, 10)],
    csv_writer:csv_writer(Durations, FileName, 0).

test_erlang(Clients) ->
    Self = self(),
    Counter = spawn(fun() -> counter(Clients, Self) end),
    Start = erlang:monotonic_time(nanosecond),
    [spawn(fun() -> start(Counter, 8001) end) || _ <- lists:seq(1, Clients)],
    receive
        {done} ->
            End = erlang:monotonic_time(nanosecond),
            Duration = (End - Start) / 1_000_000_000,
            io:format("Computation time in erlang: ~f seconds~n", [Duration])
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
