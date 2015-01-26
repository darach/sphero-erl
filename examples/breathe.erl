#!/usr/bin/env escript
%%! -pa ebin
-module(breathe).

-mode(compile).

-export([main/1]).
-export([stream/1]).

-mode(compile).

main(_) ->
  application:ensure_all_started(bt),

  {ok,[{sphero,SpheroConfig}]} = file:consult("sphero.config"),
  {ok,_S} = sphero_api:start_link(SpheroConfig),
  sphero_api:connect(),
  timer:sleep(1000),

  loop(millis()).

loop(Era) ->
  B = breathe(Era),
  X = (B bsl 16) bor (B bsl 8) bor B,
  sphero_api:rgb(X,true),
  timer:sleep(10),
  loop(Era).

breathe(Era) ->
    %% @NOTE Source of this natty little formula:
    %% http://sean.voisen.org/blog/2011/10/breathing-led-with-arduino/
    trunc((math:exp(math:sin((millis()-Era)/2000.0*math:pi()))- 0.36787944)*108.0).

millis() ->
    {MegaSecs,Secs,MicroSecs} = os:timestamp(),
    trunc(((MegaSecs*1000000 + Secs)*1000000 + MicroSecs)/1000). 

