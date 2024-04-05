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
-export([start/0]).

start() ->
  {ok, Socket} = gen_tcp:connect("localhost", 8000, [{active, false}, {mode, list}]),

  ok = gen_tcp:send(Socket, "Hello from Erlang!\n"),

  case gen_tcp:recv(Socket, 0, 5000) of
    {ok, Data} ->
      io:fwrite("Received data: ~p~n", [Data]);
    {error, closed} ->
      io:fwrite("Socket closed~n");
    {error, Reason} ->
      io:fwrite("Error: ~p~n", [Reason])
  end,

  ok = gen_tcp:close(Socket).