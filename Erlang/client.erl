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
-export([start/3, test_case_1/0]).

start(Id, Timeout, Counter) ->
  {ok, Socket} = gen_tcp:connect("localhost", 8000, [{active, false}, {mode, list}]),

  ok = gen_tcp:send(Socket, "Hello from Erlang!\n"),

  case gen_tcp:recv(Socket, 0, Timeout * 1000) of
    {ok, Data} ->
      % io:fwrite("Client: ~p Received data: ~p~n", [Id, Data]);
      ok;
    {error, closed} ->
      io:fwrite("Client : ~p Socket closed~n", [Id]);
    {error, Reason} ->
      % io:fwrite("Client: ~p Error: ~p~n", [Id, Reason]),
      Counter ! {timeout}
  end,

  ok = gen_tcp:close(Socket).

test_case_1() ->
  Pid = spawn(fun() -> counter(0) end),
  [spawn(client, start, [Id, 10, Pid]) || Id <- lists:seq(1, 400)],
  ok.
  

counter(N) -> 
  receive 
    {timeout} -> 
      counter(N + 1)
  after 7000 -> 
          io:format("Amount of timeouts : ~p~n", [N])
  end.

