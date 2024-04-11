-module(long_living_client).
-export([start/3]).

start(Id, Counter, Port) ->
  case gen_tcp:connect("localhost", Port, [{active, false}, {mode, list}]) of
    {ok, Socket} ->
      loop(Socket, Counter);
    {error, _} ->
      start(Id, Counter, Port)
  end.

loop(Socket, 0) ->
  io:format("Finished pingponging"),
  ok = gen_tcp:close(Socket);

loop(Socket, Counter) ->
      ok = gen_tcp:send(Socket, "ping"),
      case gen_tcp:recv(Socket, 0) of
        {ok, "pong"} ->
          receive
          after 10000 ->
                  loop(Socket, Counter-1)
          end;
        {error, closed} ->
          io:fwrite("Socket closed~n");
        {error, Reason} ->
          io:fwrite("Error, Reason: ~p~n", [Reason])
      end.
