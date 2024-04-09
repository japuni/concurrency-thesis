%%%-------------------------------------------------------------------
%%% @author Felix
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. apr. 2024 14:12
%%%-------------------------------------------------------------------
-module(client).
-author("Felix").

%% API
-export([start/2, test_case_1/1]).

start(Id, Counter) ->

  case gen_tcp:connect("localhost", 8000, [{active, false}, {mode, list}]) of
    {ok, Socket} ->
      ok = gen_tcp:send(Socket, "Hello from Erlang!\n"),

      case gen_tcp:recv(Socket, 0) of
        {ok, "ok"} ->
          Counter ! {done};
        {error, closed} ->
          io:fwrite("Client : ~p Socket closed~n", [Id]);
        {error, _} ->
          Counter ! {timeout}
      end,

      ok = gen_tcp:close(Socket);
    {error, _} ->
      start(Id, Counter)
  end.



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


start_counter(0) ->
  io:format("starting done");
start_counter(N) ->
  receive
    {done} ->
      start_counter(N - 1);
    {started} ->
      start_counter(N-1)

  end.
counter(0,Master) ->
  Master ! {done};
counter(N,Master) ->
  receive
    {done} ->
      counter(N - 1, Master);
    {started} ->
      counter(N-1, Master)
  end.
