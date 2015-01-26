#!/usr/bin/env escript
%%! -pa ebin
-module(accel).

-mode(compile).

-export([main/1]).

-mode(compile).

stream({stream, 16#3, << X:16/signed, Y:16/signed, Z:16/signed >>}) ->
    io:format("Gyro X: ~p Y: ~p Z: ~p~n", [X, Y, Z]);
stream({stream, 16#8, Data}) ->
    io:format("OrbBasic> ~p~n", [binary_to_list(Data)]).

-define(debug,1).
main(_) ->
  application:ensure_all_started(bt),

  {ok,[{sphero,SpheroConfig}]} = file:consult("sphero.config"),
  {ok,_S} = sphero_api:start_link(SpheroConfig),
  sphero_api:connect(),

  sphero_api:stabilization(false),

  sphero_api:stream(100, 1, 16#1C000000, 0, fun stream/1),

  io:format("Sphero: Gryo X.Y.Z~n", []),
  loop(0).

loop(Seq) ->
  timer:sleep(10),
  loop(Seq+1).
