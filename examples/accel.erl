#!/usr/bin/env escript
%%! -pa ebin
-module(accel).

-mode(compile).

-export([main/1]).

-mode(compile).

main(_) ->
  application:ensure_all_started(bt),

  {ok,[{sphero,SpheroConfig}]} = file:consult("sphero.config"),
  {ok,_S} = sphero_api:start_link(SpheroConfig),
  sphero_api:connect(),

  timer:sleep(3000),

  sphero_api:stabilization(false),

  sphero_api:stream(100, 1, 16#E0000000, 0, fun({stream, 16#3, << X:16/signed, Y:16/signed, Z:16/signed >>}) ->
      io:format("X: ~p Y: ~p Z: ~p~n", [X, Y, Z])
  end),

  io:format("Sphero: Accelerometer X.Y.Z~n", []),
  loop(0).

loop(Seq) ->
  timer:sleep(10),
  loop(Seq+1).
