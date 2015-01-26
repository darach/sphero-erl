#!/usr/bin/env escript
%%! -pa ebin
%% -------------------------------------------------------------------
%% Copyright (c) 2015 Darach Ennis < darach at gmail dot com >
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the
%% "Software"), to deal in the Software without restriction, including
%% without limitation the rights to use, copy, modify, merge, publish,
%% distribute, sublicense, and/or sell copies of the Software, and to permit
%% persons to whom the Software is furnished to do so, subject to the
%% following conditions:
%%
%% The above copyright notice and this permission notice shall be included
%% in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
%% OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
%% NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
%% DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
%% OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
%% USE OR OTHER DEALINGS IN THE SOFTWARE.
%%
%% File: ob.erl. Orb Basic console script
%%
%% -------------------------------------------------------------------
-module(ob).

-mode(compile).

-export([main/1]).

-define(delay, 3000).
-define(options, [
    {action,  $a, "action",  {atom, exec}, "Action. One of 'exec' or 'stop' or 'clear'"},
    {loop,  $l, "loop",  {boolean, false}, "Stay connected and provide OB output"}
]).

stream({stream, 16#8, Data}) ->
    io:format("OrbBasic> ~p~n", [binary_to_list(Data)]);
stream({stream, MRSP, Data}) ->
    io:format("Other(~p)? ~p~n", [MRSP,binary_to_list(Data)]).

main(Args) ->
    case getopt:parse(?options, Args) of
        {ok, {[{action, exec},{loop,Loop}],Filez}} ->
            run(exec,Filez),
            case Loop of true -> loop(); false -> ok end;
        {ok, {[{action, Other},{loop,false}],_DontCare}} ->
            run(Other,[]);
        _ -> usage()
    end.

loop() -> erlang:yield(), loop().

usage() ->
    getopt:usage(?options, escript:script_name(), "[<file> ...]").

run(stop,_DontCare) ->
    stop();
run(clear,_DontCare) ->
    clear();
run(exec,[File]) ->
    case file:read_file(File) of
        {ok,FileBin} -> exec(FileBin);
        _ -> usage()
    end;
run(_,_) ->
    usage().

stop() ->
  application:ensure_all_started(bt),
  {ok,[{sphero,SpheroConfig}]} = file:consult("sphero.config"),
  {ok,_S} = sphero_api:start_link(SpheroConfig),
  sphero_api:connect(),
  timer:sleep(?delay),
  sphero_api:ob_abort().

clear() ->
  application:ensure_all_started(bt),
  {ok,[{sphero,SpheroConfig}]} = file:consult("sphero.config"),
  {ok,_S} = sphero_api:start_link(SpheroConfig),
  sphero_api:connect(),
  timer:sleep(?delay),
  sphero_api:ob_abort(),
  sphero_api:ob_erase(ram),
  sphero_api:backled(0),
  sphero_api:rgb(0,true).


exec(FileBin) ->
  application:ensure_all_started(bt),
  {ok,[{sphero,SpheroConfig}]} = file:consult("sphero.config"),
  {ok,_S} = sphero_api:start_link(SpheroConfig),
  sphero_api:connect(),

  timer:sleep(?delay),

  sphero_api:stream(100, 1, 0, 0, 0, fun stream/1),

  sphero_api:ob_erase(ram),
  OrbBasicScriptSize = byte_size(FileBin),
  sphero_api:ob_append(ram,
    binary:replace(
      << FileBin:OrbBasicScriptSize/binary, 0:8>>, << 13:8 >>, << 10:8 >>, [global])),
  sphero_api:ob_exec(ram,10).

