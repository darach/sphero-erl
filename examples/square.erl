#!/usr/bin/env escript
%%! -pa ebin
-module(square).

-mode(compile).

-export([main/1]).

-define(delay,1000).

main(_) ->
  application:ensure_all_started(bt),
  {ok,[{sphero,SpheroConfig}]} = file:consult("sphero.config"),
  {ok,_S} = sphero_api:start_link(SpheroConfig),
  sphero_api:connect(),

  delay(?delay),

  squares().

delay(Ms) ->
  timer:sleep(Ms).

squares() ->
  sphero_api:rgb(16#FF0000, true),
  sphero_api:roll(200, 0),
  delay(500),
  sphero_api:roll(0, 0),
  delay(500),
  sphero_api:rgb(16#00FF00, true),
  sphero_api:roll(200, 90),
  delay(500),
  sphero_api:roll(0, 90),
  delay(500),
  sphero_api:rgb(16#0000FF, true),
  sphero_api:roll(200, 180),
  delay(500),
  sphero_api:roll(0, 180),
  delay(500),
  sphero_api:rgb(16#00FFFF, true),
  sphero_api:roll(200, 270),
  delay(500),
  sphero_api:roll(0, 270),
  delay(500),
  sphero_api:roll(0, 0),
  squares().
