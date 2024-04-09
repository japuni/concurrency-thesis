-module(client).
-author("Felix").

%% API
-export([test_java/1, test_erlang/1, test_case_1/1]).

start(Id, Counter, Port) ->

  case gen_tcp:connect("localhost", Port, [{active, false}, {mode, list}]) of
    {ok, Socket} ->
      ok = gen_tcp:send(Socket, "Hello from Erlang!\n"),

      case gen_tcp:recv(Socket, 0) of
        {ok, "ok"} ->
          Counter ! {done};
        {error, closed} ->
          io:fwrite("Client : ~p Socket closed~n", [Id]);
        {error, Reason} ->
          io:fwrite("Client ~p: Error, Reason: ~p~n", [Id, Reason])
      end,
      ok = gen_tcp:close(Socket);
    {error, _} ->

      start(Id, Counter, Port)
  end.


test_java(N) ->
  Self = self(),
  Pid = spawn(fun() -> counter(N , Self) end),
  Start = erlang:monotonic_time(nanosecond),
  [spawn(fun() -> start(Id, Pid, 8000) end) || Id <- lists:seq(1, N)],
  receive
    {done} ->
      End = erlang:monotonic_time(nanosecond),
      Duration = (End - Start) / 1_000_000_000,
          io:format("Computation time in java: ~f seconds~n", [Duration]),
          io:format("Throughput in java: ~f seconds~n", [(N / Duration)])
  end,
  ok.

test_erlang(N) ->
  Self = self(),
  Pid = spawn(fun() -> counter(N , Self) end),
  Start = erlang:monotonic_time(nanosecond),
  [spawn(fun() -> start(Id, Pid, 8001) end) || Id <- lists:seq(1, N)],
  receive
    {done} ->
      End = erlang:monotonic_time(nanosecond),
      Duration = (End - Start) / 1_000_000_000,
          io:format("Computation time in erlang: ~f seconds~n", [Duration]),
          io:format("Throughput in erlang: ~f kOps~n", [(N / Duration) /1000])
  end,
  ok.

test_case_1(N) ->
  Self = self(),
  Pid = spawn(fun() -> counter(N , Self) end),
  Start = erlang:monotonic_time(nanosecond),
  [spawn(client, start, [Id, Pid]) || Id <- lists:seq(1, N)],
  receive
    {done} ->
      End = erlang:monotonic_time(nanosecond),
      Duration = (End - Start) / 1_000_000_000,
      receive
        after 2000 ->
          io:format("Computation time (sequential): ~f seconds~n", [Duration])
      end
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
