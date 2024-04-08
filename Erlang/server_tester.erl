-module(server_tester).
-export([start/1]).
-define(CLIENT, client).
-define(SERVER, server).

start(N) ->
  spawn(?SERVER, start, []),
  [spawn(?CLIENT, start, []) || _ <- lists:seq(1, N)].


