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
%% File: sphero_pkt.erl. Sphero API packets
%%
%% -------------------------------------------------------------------

-module(sphero_pkt).

-export([packetize/1]).

-export([mask/2]).

% Core
-export([ping/1]).
-export([version/1]).
-export([control_uart_tx_line/2]).
-export([device_name/2]).
-export([bluetooth_info/1]).
-export([auto_reconnect/3]).
-export([auto_reconnect/1]).
-export([power_state/1]).
-export([power_notification/2]).

-export([sleep/4]).
-export([voltage_trip_points/1]).
-export([voltage_trip_points/3]).
-export([inactivity_timeout/2]).
-export([jump_to_bootloader/1]).
-export([diag1/1]).
-export([diag2/1]).
-export([clear_counters/1]).
-export([assign_time_value/2]).
-export([poll_packet_times/2]).

% API
-export([heading/2]).
-export([stabilization/2]).
-export([rotation_rate/2]).
-export([put_config/2]).
-export([get_config/1]).
-export([chassis_id/1]).
%-export([chassis_id/2]).
-export([self_level/8]).
-export([stream/6]).
-export([collision_detection/7]).
-export([locator/5]).
-export([accelerometer_range/2]).
-export([locator/1]).
-export([rgb/3]).
-export([backled/2]).
-export([rgb/1]).
-export([roll/4]).
-export([timed_boost/3]).
-export([motors/5]).
-export([motion_timeout/2]).
-export([permanent_options/2]).
-export([permanent_options/1]).
-export([temporary_options/2]).
-export([temporary_options/1]).
-export([config_block/2]).
-export([device_mode/2]).
-export([set_config_block/2]).
-export([device_mode/1]).

% Macros
-export([run_macro/2]).
-export([save_temp_macro/2]).
-export([save_macro/2]).
-export([reinit_macro_exec/1]).
-export([abort_macro/1]).
-export([macro_status/1]).
-export([set_macro_param/3]).
-export([append_macro_chunk/2]).
-export([erase_ob_storage/2]).
-export([append_ob_storage/3]).
-export([exec_ob_storage/3]).
-export([abort_ob_storage/1]).
-export([submit_value/2]).

% ORB basic

-define(mask, [
  %% {bit, sensor, range, units}
  {16#80000000, [{sensor,"accelerometer axis X, raw"}, {range,"-2048 to 2047"}, {unit,"4mG"}]},
  {16#40000000, [{sensor,"accelerometer axis Y, raw"}, {range,"-2048 to 2047"}, {unit,"4mG"}]},
  {16#20000000, [{sensor,"accelerometer axis Z, raw"}, {range,"-2048 to 2047"}, {unit,"4mG"}]},
  {16#10000000, [{sensor,"gyro axis X, raw"}, {range,"-32768 to 32767"},{unit,"0.068 degrees"}]},
  {16#08000000, [{sensor,"gyro axis Y, raw"}, {range,"-32768 to 32767"},{unit,"0.068 degrees"}]},
  {16#04000000, [{sensor,"gyro axis Z, raw"}, {range,"-32768 to 32767"},{unit,"0.068 degrees"}]},
  {16#02000000, [{sensor,"Reserved"}, {range,undefined}, {unit,undefined}]},
  {16#01000000, [{sensor,"Reserved"}, {range,undefined}, {unit,undefined}]},
  {16#00800000, [{sensor,"Reserved"}, {range,undefined}, {unit,undefined}]},
  {16#00400000, [{sensor,"right motor back EMF, raw"},{range,"-32768 to 32767"},{unit,"22.5cm"}]},
  {16#00200000, [{sensor,"left motor back EMF, raw"},{range,"-32768 to 32767"},{unit,"22.5cm"}]},
  {16#00100000, [{sensor,"left motor, PWM, raw"}, {range, "-2048 to 2047"},{unit,"duty cycle"}]},
  {16#00080000, [{sensor,"right motor, PWM raw"},{range, "-2048 to 2047"},{unit,"duty cycle"}]},
  {16#00040000, [{sensor,"IMU pitch angle, filtered"},{range, "-179 to 180"}, {unit,"degrees"}]},
  {16#00020000, [{sensor,"IMU roll angle, filtered"}, {range, "-179 to 180"}, {unit,"degrees"}]},
  {16#00010000, [{sensor,"IMU yaw angle, filtered"},{range, "-179 to 180"}, {unit,"degrees"}]},
  {16#00008000, [{sensor,"accelerometer axis X, filtered"}, {range,  "-32768 to 32767"}, {unit, "1/4096 G"}]},
  {16#00004000, [{sensor,"accelerometer axis Y, filtered"}, {range,  "-32768 to 32767"}, {unit, "1/4096 G"}]},
  {16#00002000, [{sensor,"accelerometer axis Z, filtered"}, {range,  "-32768 to 32767"}, {unit, "1/4096 G"}]},
  {16#00001000, [{sensor,"gyro axis X, filtered"}, {range,  "-20000 to 20000"}, {unit, "0.1 dps"}]},
  {16#00000800, [{sensor,"gyro axis Y, filtered"}, {range,  "-20000 to 20000"}, {unit, "0.1 dps"}]},
  {16#00000400, [{sensor,"gyro axis Z, filtered"}, {range,  "-20000 to 20000"}, {unit, "0.1 dps"}]},
  {16#00000200, [{sensor,"Reserved"}, {range, undefined}, {unit,undefined}]},
  {16#00000100, [{sensor,"Reserved"}, {range, undefined}, {unit,undefined}]},
  {16#00000080, [{sensor,"Reserved"}, {range, undefined}, {unit,undefined}]},
  {16#00000040, [{sensor,"right motor backEMF, filtered"},{range,"-32768 to 32767"},{unit,"22.5cm"}]},
  {16#00000020, [{sensor,"left motor backEMF, filtered"},{range,"-32768 to 32767"},{unit,"22.5cm"}]},
  {16#00000010, [{sensor,"Reserved 1"}, {range,undefined}, {unit,undefined}]},
  {16#00000008, [{sensor,"Reserved 2"}, {range,undefined}, {unit,undefined}]},
  {16#00000004, [{sensor,"Reserved 3"}, {range,undefined}, {unit,undefined}]},
  {16#00000002, [{sensor,"Reserved 4"}, {range,undefined}, {unit,undefined}]},
  {16#00000001, [{sensor,"Reserved 5"}, {range,undefined}, {unit,undefined}]}
]).

-define(mask2, [
  %% {bit, sensor, range, units}
  {16#80000000, [{sensor,"Quaternion Q0"}, {range,"-10000 to 10K"}, {unit,"1/10000 Q"}]},
  {16#40000000, [{sensor,"Quaternion Q1"}, {range,"-10000 to 10000"}, {unit,"1/10000 Q"}]},
  {16#20000000, [{sensor,"Quaternion Q2"}, {range,"-10000 to 10000"}, {unit,"1/10000 Q"}]},
  {16#10000000, [{sensor,"Quaternion Q3"}, {range,"-10000 to 10000"}, {unit,"1/10000 Q"}]},
  {16#08000000, [{sensor,"Odometer X"}, {range,"-32768 to 32767"}, {unit,"cm"}]},
  {16#04000000, [{sensor,"Odometer Y"}, {range,"-32768 to 32767"}, {unit,"cm"}]},
  {16#02000000, [{sensor,"AccelOne"}, {range,"0 to 8000"}, {unit,"1 mG"}]},
  {16#01000000, [{sensor,"Velocity X"}, {range,"-32768 to 32767"}, {unit,"mm/s"}]},
  {16#00800000, [{sensor,"Velocity Y"}, {range,"-32768 to 32767"}, {unit,"mm/s"}]}
]).

-define(no_data, << >>).

-record(command, {
  did :: binary(),    %% Device ID
  cid :: binary(),    %% Command ID
  seq :: binary(),    %% Sequence number
  data = << >> :: binary(),
  reset_timeout = false:: boolean(),
  acknowledge = true :: boolean()
}).

bit(true) -> 1;
bit(false) -> 0.

cksum(Data) -> cksum(Data, 0).
cksum(<<I, T/binary>>, Acc) -> cksum(T,  I + Acc);
cksum(<<>>, Acc) -> (Acc band 16#FF) bxor 16#FF.

mask(Stream,Mask) -> mask(Stream,Mask,0).
mask([H|T],Mask,Cnt) -> 
  {Bit,[{sensor,Sensor},{range,Range},{unit,Unit}]} = H,
  case Mask band Bit > 0 of
    true -> 
      io:format("Streaming data for sensor ~p with range ~p and units ~p", [Sensor, Range, Unit]),
      mask(T,Mask,Cnt+1);
    false -> mask(T,Mask,Cnt)
  end;
mask([],_,Cnt) -> Cnt.

packetize(#command{did=DID,cid=CID,seq=SEQ,data=Data,reset_timeout=RT,acknowledge=AK}) ->
  SOP1 = 16#FF,
  SOP2 = 16#FC bor (bit(RT) band 16#02) bor (bit(AK) band 16#01),
  DataLen = byte_size(Data),
  PayloadLen = DataLen+1,
  case DataLen > 255 of
    true -> throw(etoobig);
    false -> 
      InnerPayload = << DID:8, CID:8, SEQ:8, PayloadLen:8, Data/binary >>,
      Checksum = cksum(InnerPayload),
      << SOP1:8, SOP2:8, DID:8, CID:8, SEQ:8, PayloadLen:8, Data/binary, Checksum:8>>
  end.

%%
%% Core
%%

ping(Seq) ->
  {ping, packetize(#command{did=16#00,cid=16#01,seq=Seq,data=?no_data,acknowledge=true})}.

version(Seq) ->
  {version, packetize(#command{did=16#00,cid=16#02,seq=Seq,data=?no_data,acknowledge=true})}.

device_name(Seq,Name) when is_list(Name) ->
  Data = list_to_binary(Name),
  {device_name, packetize(#command{did=16#00,cid=16#10,seq=Seq,data=Data,acknowledge=true})}.

control_uart_tx_line(Seq,true=_Enable) ->
  Data = << 1:8 >>,
  packetize(#command{did=16#00,cid=16#03,seq=Seq,data=Data,acknowledge=true});
control_uart_tx_line(Seq,false=_Enable) ->
  Data = << 0:8 >>,
  {control_uart_tx_line,packetize(#command{did=16#00,cid=16#03,seq=Seq,data=Data,acknowledge=true})}.

bluetooth_info(Seq) ->
  {bluetooth_info,packetize(#command{did=16#00,cid=16#11,seq=Seq,data=?no_data,acknowledge=true})}.

auto_reconnect(Seq,Enable,Time) ->
  Data = << (bit(Enable)):8, Time:8>>,
  {auto_reconnect,packetize(#command{did=16#00,cid=16#12,seq=Seq,data=Data,acknowledge=true})}.

auto_reconnect(Seq) ->
  {auto_reconnect,packetize(#command{did=16#00,cid=16#13,seq=Seq,data=?no_data,acknowledge=true})}.

power_state(Seq) ->
  {power_state,packetize(#command{did=16#00,cid=16#20,seq=Seq,data=?no_data,acknowledge=true})}.

power_notification(Seq,Enable) ->
  Data = <<(bit(Enable)):8 >>,
  {power_notification,packetize(#command{did=16#00,cid=16#21,seq=Seq,data=Data,acknowledge=true})}.

sleep(Seq,Wakeup,Macro,OrbBasic) ->
  Data = << Wakeup:16, Macro:8, OrbBasic:16 >>,
  {sleep,packetize(#command{did=16#00,cid=16#22,seq=Seq,data=Data,acknowledge=true})}.

voltage_trip_points(Seq) ->
  {voltage_trip_points,packetize(#command{did=16#00,cid=16#23,seq=Seq,data=?no_data,acknowledge=true})}.

voltage_trip_points(Seq,LowVoltage,CriticalVoltage) ->
  Data = << LowVoltage:16, CriticalVoltage:16 >>,
  {voltage_trip_points, packetize(#command{did=16#00,cid=16#24,seq=Seq,data=Data,acknowledge=true})}.

inactivity_timeout(Seq,Time) ->
  Data = << Time:16 >>,
  {inactivity_timeout,packetize(#command{did=16#00,cid=16#25,seq=Seq,data=Data,acknowledge=true})}.

jump_to_bootloader(Seq) ->
  {jump_to_bootloader,packetize(#command{did=16#00,cid=16#30,seq=Seq,data=?no_data,acknowledge=true})}.

diag1(Seq) ->
  {diag1,packetize(#command{did=16#00,cid=16#40,seq=Seq,data=?no_data,acknowledge=true})}.

diag2(Seq) ->
  {diag2,packetize(#command{did=16#00,cid=16#41,seq=Seq,data=?no_data,acknowledge=true})}.

clear_counters(Seq) ->
  {clear_counters,packetize(#command{did=16#00,cid=16#42,seq=Seq,data=?no_data,acknowledge=true})}.

assign_time_value(Seq,Time) ->
  Data = << Time:32 >>,
  {assign_time_value,packetize(#command{did=16#00,cid=16#23,seq=Seq,data=Data,acknowledge=true})}.

poll_packet_times(Seq,Time) ->
  Data = << Time:32 >>,
  {poll_packet_times,packetize(#command{did=16#00,cid=16#51,seq=Seq,data=Data,acknowledge=true})}.

%%
%% API
%%  

heading(Seq,Heading) ->
  Data = << Heading:16 >>,
  {heading,packetize(#command{did=16#02,cid=16#01,seq=Seq,data=Data,acknowledge=true})}.

stabilization(Seq,Enable) ->
  Data = << (bit(Enable)):16 >>,
  {heading,packetize(#command{did=16#02,cid=16#02,seq=Seq,data=Data,acknowledge=true})}.

rotation_rate(Seq,Rate) ->  
  Data = << Rate:8 >>,
  {rotation_rate,packetize(#command{did=16#02,cid=16#03,seq=Seq,data=Data,acknowledge=true})}.

put_config(Seq,Data) when is_binary(Data) ->  
  32 = byte_size(Data),
  {put_config,packetize(#command{did=16#02,cid=16#04,seq=Seq,data=Data,acknowledge=true})}.

get_config(Seq) ->  
  {get_config,packetize(#command{did=16#02,cid=16#05,seq=Seq,data=?no_data,acknowledge=true})}.

chassis_id(Seq) ->  
  {chassis_id,packetize(#command{did=16#02,cid=16#07,seq=Seq,data=?no_data,acknowledge=true})}.

%chassis_id(Seq,ChassisId) when is_binary(Data) ->  
%  Data = << ChassisId:16 >>,
%  {chassis_id,packetize(#command{did=16#02,cid=16#08,seq=Seq,data=Data,acknowledge=true})}.

self_level(Seq,StartStop,FinalAngle,Sleep,ControlSystem,AngleLimit,Timeout,TrueTime) ->
  Data = << 
    (bit(StartStop)):8, (bit(FinalAngle)):8, (bit(Sleep)):8, (bit(ControlSystem)):8,
    AngleLimit:8, Timeout:8, TrueTime:8
  >>,
  {self_level,packetize(#command{did=16#02,cid=16#09,seq=Seq,data=Data,acknowledge=true})}.

stream(Seq, SensorRateDivisor, Frames, Mask, PacketCount, Mask2) ->
  Data = case Mask2 > 0 of
    true -> << SensorRateDivisor:16, Frames:16, Mask:32, PacketCount:8, Mask2:32 >>;
    false -> << SensorRateDivisor:16, Frames:16, Mask:32, PacketCount:8 >>
  end,
  packetize(#command{did=16#02,cid=16#11,seq=Seq,data=Data,acknowledge=true}).

collision_detection(Seq, Method, ThresholdX, ThresholdY, SpeedX, SpeedY, DeadTime) ->
  Data = << Method:8, ThresholdX:8, ThresholdY:8, SpeedX:8, SpeedY:8, DeadTime:8 >>,
  {collision_detection,packetize(#command{did=16#02,cid=16#12,seq=Seq,data=Data,acknowledge=true})}.

locator(Seq, Flags, X, Y, YawTare) ->
  Data = << Flags:8, X:8, Y:8, YawTare:8 >>,
  {locator,packetize(#command{did=16#02,cid=16#13,seq=Seq,data=Data,acknowledge=true})}.

accelerometer_range(Seq, Index) ->
  Data = << Index:8 >>,
  {accelerometer_range,packetize(#command{did=16#02,cid=16#14,seq=Seq,data=Data,acknowledge=true})}.

locator(Seq) ->
  {locator,packetize(#command{did=16#02,cid=16#15,seq=Seq,data=?no_data,acknowledge=true})}.

rgb(Seq,Color,Persist) ->
  R = (Color bsr 16) band 16#FF,
  G = (Color bsr 8) band 16#FF,
  B = Color band 16#FF,
  P = bit(Persist),
  Data = << R:8, G:8, B:8, P:8 >>,
  {rgb,packetize(#command{did=16#02,cid=16#20,seq=Seq,data=Data,acknowledge=true})}.

backled(Seq,Intensity) ->
  Data = << Intensity:8 >>,
  {backled,packetize(#command{did=16#02,cid=16#21,seq=Seq,data=Data,acknowledge=true})}.

rgb(Seq) ->
  {rgb,packetize(#command{did=16#02,cid=16#22,seq=Seq,data=?no_data,acknowledge=true})}.

roll(Seq,Speed,Heading,State) ->
  Data = << Speed:8, Heading:16, State:8 >>,
  {roll,packetize(#command{did=16#02,cid=16#30,seq=Seq,data=Data,acknowledge=true})}.

timed_boost(Seq, Time, Heading) ->
  Data = << Time:8, Heading:16 >>,
  {timed_boost,packetize(#command{did=16#02,cid=16#31,seq=Seq,data=Data,acknowledge=true})}.

motors(Seq, LeftMode, LeftPower, RightMode, RightPower) ->
  Data = << LeftMode:8, LeftPower:8, RightMode:8, RightPower:8 >>,
  {motors,packetize(#command{did=16#02,cid=16#33,seq=Seq,data=Data,acknowledge=true})}.

motion_timeout(Seq,Time) ->
  Data = << Time:16 >>,
  {motion_timeout,packetize(#command{did=16#02,cid=16#34,seq=Seq,data=Data,acknowledge=true})}.

permanent_options(Seq,Flags) ->
  Data = << Flags:32 >>,
  {permanent_options,packetize(#command{did=16#02,cid=16#35,seq=Seq,data=Data,acknowledge=true})}.

permanent_options(Seq) ->
  {permanent_options,packetize(#command{did=16#02,cid=16#36,seq=Seq,data=?no_data,acknowledge=true})}.

temporary_options(Seq,Flags) ->
  Data = << Flags:32 >>,
  {temporary_options,packetize(#command{did=16#02,cid=16#37,seq=Seq,data=Data,acknowledge=true})}.

temporary_options(Seq) ->
  {temporary_options,packetize(#command{did=16#02,cid=16#38,seq=Seq,data=?no_data,acknowledge=true})}.

config_block(Seq,Id) ->
  Data = << Id:8 >>,
  {config_block,packetize(#command{did=16#02,cid=16#40,seq=Seq,data=Data,acknowledge=true})}.

device_mode(Seq,Mode) ->
  Data = << Mode:8 >>,
  {device_mode,packetize(#command{did=16#02,cid=16#42,seq=Seq,data=Data,acknowledge=true})}.

set_config_block(Seq,Data) ->
  {set_config_block,packetize(#command{did=16#02,cid=16#43,seq=Seq,data=Data,acknowledge=true})}.

device_mode(Seq) ->
  {device_mode,packetize(#command{did=16#02,cid=16#44,seq=Seq,data=?no_data,acknowledge=true})}.

%%
%% Macros
%%

run_macro(Seq,MacroId) ->
  Data = << MacroId:8 >>,
  {run_macro,packetize(#command{did=16#02,cid=16#50,seq=Seq,data=Data,acknowledge=true})}.

save_temp_macro(Seq,Macro) ->  
  case byte_size(Macro) > 254 of
    true -> throw({bad_macro,too_big});
    false -> ok
  end,
  {save_temp_macro,packetize(#command{did=16#02,cid=16#51,seq=Seq,data=Macro,acknowledge=true})}.

save_macro(Seq,Macro) ->
  case byte_size(Macro) > 254 of
    true -> throw({bad_macro,too_big});
    false -> ok
  end,
  {save_macro,packetize(#command{did=16#02,cid=16#52,seq=Seq,data=Macro,acknowledge=true})}.

reinit_macro_exec(Seq) ->
  {reinit_macro_exec,packetize(#command{did=16#02,cid=16#54,seq=Seq,data=?no_data,acknowledge=true})}.

abort_macro(Seq) ->
  {abort_macro,packetize(#command{did=16#02,cid=16#55,seq=Seq,data=?no_data,acknowledge=true})}.

macro_status(Seq) ->
  {macro_status,packetize(#command{did=16#02,cid=16#56,seq=Seq,data=?no_data,acknowledge=true})}.

set_macro_param(Seq,Param,Value) ->
  Data = case Param > 1 of
    true -> << Param:8, Value:8, 0:8 >>;
    false -> << Param:8, Value:16 >>
  end,
  {set_macro_param,packetize(#command{did=16#02,cid=16#57,seq=Seq,data=Data,acknowledge=true})}.

append_macro_chunk(Seq,Macro) ->
  case byte_size(Macro) > 254 of
    true -> throw({bad_macro,too_big});
    false -> ok
  end,
  {append_macro_chunk,packetize(#command{did=16#02,cid=16#58,seq=Seq,data=Macro,acknowledge=true})}.

erase_ob_storage(Seq,Area) ->
  Data = << Area:8 >>,
  {erase_ob_storage,packetize(#command{did=16#02,cid=16#60,seq=Seq,data=Data,acknowledge=true})}.

append_ob_storage(Seq,Area,Frag) ->
  FragSize = byte_size(Frag),
  Data = << Area:8, Frag:FragSize/binary >>,
  {append_ob_storage,packetize(#command{did=16#02,cid=16#61,seq=Seq,data=Data,acknowledge=true})}.

exec_ob_storage(Seq,Area,StartLine) ->
  Data = << Area:8, StartLine:16 >>,
  {exec_ob_storage,packetize(#command{did=16#02,cid=16#62,seq=Seq,data=Data,acknowledge=true})}.

abort_ob_storage(Seq) ->
  {abort_ob_storage,packetize(#command{did=16#02,cid=16#63,seq=Seq,data=?no_data,acknowledge=true})}.

submit_value(Seq,Value) ->
  Data = << Value:32 >>,
  {submit_value,packetize(#command{did=16#02,cid=16#64,seq=Seq,data=Data,acknowledge=true})}.
  
