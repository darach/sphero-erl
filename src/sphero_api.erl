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
%% File: sphero_api.erl. Sphero API.
%%
%% --------------------------------------------------------------------module(sphero_api).
-behaviour(gen_fsm).

-export([start_link/1]).
-export([connect/0]).
-export([disconnect/0]).
-export([command/1]).

% Core
-export([ping/0]).
-export([version/0]).
-export([control_uart_tx_line/1]).
-export([device_name/1]).
-export([bluetooth_info/0]).
-export([auto_reconnect/2]).
-export([auto_reconnect/0]).
-export([power_state/0]).
-export([power_notification/1]).

-export([sleep/3]).
-export([voltage_trip_points/0]).
-export([voltage_trip_points/2]).
-export([inactivity_timeout/1]).
-export([jump_to_bootloader/0]).
-export([diag1/0]).
-export([diag2/0]).
-export([clear_counters/0]).
-export([assign_time_value/1]).
-export([poll_packet_times/1]).

% API
-export([heading/1]).
-export([stabilization/1]).
-export([rotation_rate/1]).
-export([put_config/1]).
-export([get_config/0]).
-export([chassis_id/0]).
%-export([chassis_id/1]).
-export([self_level/7]).
-export([stream/5]).
-export([stream/6]).
-export([collision_detection/6]).
-export([locator/4]).
-export([accelerometer_range/1]).
-export([locator/0]).
-export([rgb/2]).
-export([backled/1]).
-export([rgb/0]).
-export([roll/2]).
-export([timed_boost/2]).
-export([motors/4]).
-export([motion_timeout/1]).
-export([permanent_options/1]).
-export([permanent_options/0]).
-export([temporary_options/1]).
-export([temporary_options/0]).
-export([config_block/1]).
-export([device_mode/1]).
-export([set_config_block/1]).
-export([device_mode/0]).

% Macros
-export([run_macro/1]).
-export([save_temp_macro/1]).
-export([save_macro/1]).
-export([reinit_macro_exec/0]).
-export([abort_macro/0]).
-export([macro_status/0]).
-export([set_macro_param/2]).
-export([append_macro_chunk/1]).

% OrbBasic
-export([ob_erase/1]).
-export([ob_append/2]).
-export([ob_exec/2]).
-export([ob_abort/0]).
-export([submit_value/1]).




-export([init/1]).
-export([handle_event/3]).
-export([handle_sync_event/4]).
-export([handle_info/3]).
-export([code_change/4]).
-export([terminate/3]).

-export([not_connected/2]).
-export([idle/3]).
-export([idle/2]).

-define(SERVER, ?MODULE).

-include("src/sphero.hrl").

-record(state, {
    conn = undefined :: undefined | #sphero_bt{},
    buff = << >> :: binary(),
    stream_cb,
    id,
    channel,
    serial,
    baud
}).

% Core

%% @doc Ping the sphero
ping() ->
    {message, 0, 1, _, _} = command(sphero_pkt:ping(1)),
    pong.

%% @doc Get version information
version() ->
    {message, 0, 2, Data, _} = command(sphero_pkt:version(2)),
    %% @NOTE The sphero API indicates a 10 byte version but the firmware is returning an 8 byte
    %% packet. The mapping below may be incorrect as a result...
    %%
    << RECV:8, MDL:8, HW:8, MSAver:8, MSArev:8, BLmaj:4, BLmin:4, BasMaj:4, BasMin:4, Macro:8 >> = Data,
    {version, [
        {recv, RECV},
        {model, MDL},
        {hardware, HW},
        {main_sphero_app, [{version, MSAver}, {revision, MSArev}]},
        {bootloader, [{major, BLmaj}, {minor, BLmin}]},
        {orb_basic_env, [{major, BasMaj}, {minor, BasMin}]},
        {macro_env, Macro}
        %{firmware_api, [{major, APImaj},{minor,APImin}]}
    ]}.

%% @doc Set the (bluetooth) device name
device_name(Name) ->
    {message, 0, 3, _, _} = command(sphero_pkt:device_name(3,Name)),
    ok.

%% @doc Set UART TX enable
control_uart_tx_line(Enable) ->
    {message, 0, 4, _, _} = command(sphero_pkt:control_uart_tx_line(4,Enable)),
    ok.

%% @doc Get the bluetooth name and and MAC address
bluetooth_info() ->
    {message, 0, 5, Data, _} = command(sphero_pkt:bluetooth_info(5)),
    {bluetooth_info, binary_to_list(binary:replace(Data, <<0:8>>, <<" ">>, [global]))}.

%% @doc Set auto reconnect options
auto_reconnect(Enable,Time) ->
    {message, 0, 6, _, _} = command(sphero_pkt:auto_reconnect(6,Enable,Time)),
    ok.

%% @doc Get auto reconnect options
auto_reconnect() ->
    {message, 0, 7, Data, _} = command(sphero_pkt:auto_reconnect(7)),
    << Flag:8, Time:8 >> = Data,
    {auto_reconnect, [{enable, Flag =:= 1}, {time, Time}]}.

%% @doc Get power state
power_state() ->
    Response = command(sphero_pkt:power_state(8)),
    {message, 0, 8, Data, _} = Response,
    << 01:8, PowerState:8, BattVoltage:16, NumCharges:16, TimeSinceChg:16 >> = Data,
    {power_state, [
        {recv, 1},
        {power_level, power_level(PowerState)},
        {battery_voltage, voltage(BattVoltage)},
        {num_charges, NumCharges},
        {time_since_last_charge, TimeSinceChg}
    ]}.

power_level(1) -> charging;
power_level(2) -> ok;
power_level(3) -> low;
power_level(4) -> critical.
voltage(X) -> X / 100.0. %% convert data unit of 100ths of a volt -> volt (api unit)

%% @doc Set power notifications
power_notification(Enable) ->
    {message, 0, 9, _, _} = command(sphero_pkt:power_notification(9,Enable)),
    ok.

%% @doc Set sleep settings
sleep(Wakeup,Macro,OrbBasic) ->
    {message, 0, 10, _, _} = command(sphero_pkt:sleep(10,Wakeup,Macro,OrbBasic)),
    ok.

%% @doc Get voltage trip points
voltage_trip_points() ->
    {message, 0, 11, Data, _} = command(sphero_pkt:voltage_trip_points(11)),
    << VoltageLow:16, VoltageCritical:16 >> = Data,
    {voltage_trip_points,  [{low, voltage(VoltageLow)}, {critical, voltage(VoltageCritical)}]}.

%% @doc Set voltage trip points
voltage_trip_points(VoltageLow, VoltageCritical) ->
    {message, 0, 12, _, _} = command(sphero_pkt:voltage_trip_points(12, VoltageLow, VoltageCritical)),
    ok.

%% @doc Set inactivity timeout
inactivity_timeout(Time) ->
    {message, 0, 13, _, _} = command(sphero_pkt:inactivity_timeout(13,Time)),
    ok.

%% @doc Jump to the bootloader segment
jump_to_bootloader() ->
    {message, 0, 14, _, _} = command(sphero_pkt:jump_to_bootloader(14)),
    ok.

%% @doc Get Level 1 Performance Diagnostics
diag1() ->
    command(sphero_pkt:diag1(15)).

%% @doc Get Level 2 Performance Diagnostics
diag2() ->
    {message, 0, 16, _, _} = command(sphero_pkt:diag2(16)),
    ok.

%% @doc Clear counters
clear_counters() ->
    {message, 0, 17, _, _} = command(sphero_pkt:clear_counters(17)),
    ok.

%% @doc Assign time value
assign_time_value(Time) ->
    {message, 0, 18, _, _} = command(sphero_pkt:clear_counters(18,Time)),
    ok.

%% @doc Set packet poll times
poll_packet_times(Time) ->
    {message, 0, 19, _, _} = command(sphero_pkt:clear_counters(19,Time)),
    ok.

%% @doc Set heading (0..359 compass heading, 0 is forward)
heading(Heading) ->
    {message, 0, 20, _, _} = command(sphero_pkt:heading(20,Heading)),
    ok.

%% @doc Set stabilization
stabilization(Enable) ->
    {message, _, 21, _, _} = command(sphero_pkt:stabilization(21,Enable)),
    ok.

%% @doc Set device rotation rate
rotation_rate(Rate) ->
    {message, 0, 22, _, _} = command(sphero_pkt:rotation_rate(22,Rate)),
    ok.

%% @doc Put configuration
put_config(Data) ->
    {message, 0, 23, _, _} = command(sphero_pkt:put_config(23,Data)),
    ok.

%% @doc Get configuration
get_config() ->
    {message, 0, 24, Data, _} = command(sphero_pkt:get_config(24)),
    {config, Data}.

%% @doc Get Chassis ID
chassis_id() ->
    {message, 0, 25, Data, _} = command(sphero_pkt:chassis_id(25)),
    {chassis_id, Data}.

%chassis_id(ChassidId) ->
%    {message, 0, 25, _, _} = command(sphero_pkt:chassis_id(25,ChassisId)),
%    ok.

%% @doc Set self levels
self_level(StartStop,FinalAngle,Sleep,ControlSystem,AngleLimit,Timeout,TrueTime) ->
    {message, 0, 26, _, _} = command(sphero_pkt:self_level(26,StartStop,FinalAngle,Sleep,ControlSystem,AngleLimit,Timeout,TrueTime)),
    ok.

%% @doc Configure asynchronous streaming and callback
stream(SensorRateDivisor, Frames, Mask, PacketCount, Fun) ->
    stream_command(sphero_pkt:stream(27,SensorRateDivisor,Frames,Mask,PacketCount,0), Fun),
    ok.

%% @doc Configure asynchronous streaming and callback
stream(SensorRateDivisor, Frames, Mask, PacketCount, Mask2, Fun) ->
    stream_command(sphero_pkt:stream(27,SensorRateDivisor,Frames,Mask,PacketCount,Mask2), Fun),
    ok.

%% @doc Set collision detection parameters
collision_detection(Method,ThresholdX, ThresholdY, SpeedX, SpeedY, DeadTime) ->
    {message, 0, 28, _, _} = command(sphero_pkt:collision_detection(28,Method,ThresholdX, ThresholdY, SpeedX, SpeedY, DeadTime)),
    ok.

%% @doc Set locator parameters
locator(Flags, X, Y, YawTare) ->
    {message, 0, 29, _, _} = command(sphero_pkt:locator(29, Flags, X, Y, YawTare)),
    ok.

%% @doc Set accelerometer range
accelerometer_range(Index) ->
    {message, 0, 30, _, _} = command(sphero_pkt:accelerometer_range(30, Index)),
    ok.

%% @doc Get locator configuration
locator() ->
    {message, 0, 31, Data, _} = command(sphero_pkt:locator(31)),
    {locator, Data}.

%% @doc Set main RGB color
rgb(Color,Persist) ->
    {message, 0, 32, _, _} = command(sphero_pkt:rgb(32, Color, Persist)),
    ok.

%% @doc Set heading LED intensity
backled(Intensity) ->
    {message, 0, 33, _, _} = command(sphero_pkt:backled(33, Intensity)),
    ok.

%% @doc Get current main RGB color
rgb() ->
    {message, 0, 34, Data, _} = command(sphero_pkt:rgb(34)),
    {rgb, Data}.

%% @doc Roll device to heading at speed
roll(Speed, Heading) ->
    command(sphero_pkt:roll(35, Speed, Heading, 3)),
    ok.
%% @doc Timed boost
timed_boost(Time, Heading) ->
    {message, 0, 36, _, _} = command(sphero_pkt:timed_boost(36, Time, Heading)),
    ok.

%% @doc Drive left and right motors directly
motors(LeftMode, LeftPower, RightMode, RightPower) ->
    {message, 0, 37, _, _} = command(sphero_pkt:motors(37, LeftMode, LeftPower, RightMode, RightPower)),
    ok.

%% @doc Set motion timeout
motion_timeout(Time) ->
    {message, 0, 38, _, _} = command(sphero_pkt:motion_timeout(38, Time)),
    ok.

%% @doc Set permanent options
permanent_options(Flags) ->
    {message, 0, 39, _, _} = command(sphero_pkt:permanent_options(39, Flags)),
    ok.

%% @doc Get permanent options
permanent_options() ->
    {message, 0, 40, Data, _} = command(sphero_pkt:permanent_options(40)),
    {permanent_options, Data}.

%% @doc Set temporary options
temporary_options(Flags) ->
    {message, 0, 41, _, _} = command(sphero_pkt:temporary_options(40,Flags)),
    ok.

%% @doc Get temporary options
temporary_options() ->
    {message, 0, 42, Data, _} = command(sphero_pkt:temporary_options(42)),
    {temporary_options, Data}.

%% @doc Get configuration block by Id
config_block(Id) ->
    {message, 0, 43, _, _} = command(sphero_pkt:config_block(43,Id)),
    ok.

%% @doc Set device mode
device_mode(Mode) ->
    {message, 0, 44, _, _} = command(sphero_pkt:device_mode(44,Mode)),
    ok.

%% @doc Set configuraiton block
set_config_block(Data) ->
    {message, 0, 45, _, _} = command(sphero_pkt:set_config_block(45,Data)),
    ok.

%% @doc Get device mode
device_mode() ->
    {message, 0, 46, Data, _} = command(sphero_pkt:device_mode(46)),
    {device_mode, Data}.

%% @doc Run macro by id
run_macro(MacroId) -> %%%
    {message, 0, 47, _, _} = command(sphero_pkt:run_macro(47,MacroId)),
    ok.

%% @doc Save temporary macro
save_temp_macro(Macro) ->
    {message, 0, 48, _, _} = command(sphero_pkt:save_temp_macro(48,Macro)),
    ok.

%% @doc Save macro
save_macro(Macro) ->
    {message, 0, 49, _, _} = command(sphero_pkt:save_macro(49,Macro)),
    ok.

%% @doc Reset macro
reinit_macro_exec() ->
    {message, 0, 50, _, _} = command(sphero_pkt:save_macro(50)),
    ok.

%% @doc Abort macro
abort_macro() ->
    {message, 0, 51, _, _} = command(sphero_pkt:save_macro(51)),
    ok.

%% @doc Get macro status
macro_status() ->
    {message, 0, 52, _, _} = command(sphero_pkt:macro_status(52)),
    ok.

%% @doc Set macro parameter
set_macro_param(Param,Value) ->
    {message, 0, 53, _, _} = command(sphero_pkt:set_macro_param(53,Param,Value)),
    ok.

%% @doc Append to a macro
append_macro_chunk(Macro) ->
    {message, 0, 54, _, _} = command(sphero_pkt:append_macro_chunk(54,Macro)),
    ok.

%% @doc Erase Orb Basic code from RAM or ROM
ob_erase(ram) ->
    command(sphero_pkt:erase_ob_storage(55,0)),
    ok;
ob_erase(rom) ->
    command(sphero_pkt:erase_ob_storage(56,1)),
    ok.

%% @doc Append Orb Basic code to RAM or ROM
ob_append(ram,Fragment) ->
    {message, 0, 57, _, _} = command(sphero_pkt:append_ob_storage(57,0,Fragment)),
    ok;
ob_append(rom,Fragment) ->
    {message, 0, 58, _, _} = command(sphero_pkt:append_ob_storage(58,1,Fragment)),
    ok.

%% @doc Execute Orb Basic code in RAM or ROM
ob_exec(ram,StartLine) ->
    command(sphero_pkt:exec_ob_storage(59,0,StartLine)),
    ok;
ob_exec(rom,StartLine) ->
    command(sphero_pkt:exec_ob_storage(60,1,StartLine)),
    ok.

%% @doc Abort running Orb Basic code
ob_abort() ->
    {message, 0, 61, _, _} = command(sphero_pkt:abort_ob_storage(61)),
    ok.

submit_value(Value) ->
    {message, 0, 62, _, _} = command(sphero_pkt:submit_value(62,Value)),
    ok.


start_link(Options) ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, Options, []).

connect() ->
    gen_fsm:send_event(?SERVER, connect).

disconnect() ->
    gen_fsm:send_event(?SERVER, disconnect).

command(Command) ->
    gen_fsm:sync_send_event(?SERVER, {command, Command}).

stream_command(Command, Fun) ->
    gen_fsm:sync_send_event(?SERVER, {stream, Command, Fun}).

init([{id,Id},{channel,Channel},{serial,Serial},{baud,Baud}]) ->
    {ok, not_connected, #state{id=Id,channel=Channel,serial=Serial,baud=Baud}}.

not_connected(connect, #state{id=Id,channel=Channel,serial=Serial,baud=Baud}=State) ->
    Sphero = sphero_bt:connect(Id,Channel,Serial,Baud),
    {next_state, idle, State#state{conn=Sphero}}.

idle({command, {_CommandName, CommandBin}}, _From, #state{conn=Sphero,buff=Buffer,stream_cb=Fun}=State) ->
    Sphero#sphero_bt.serial ! {send, CommandBin},
    CommandResponse = response(Buffer),  
    {message, MRSP, _, Data, More} = CommandResponse,
    case MRSP of
        8 -> Fun({stream,8,Data});
        _ -> ok
    end,
    {reply, CommandResponse, idle, State#state{buff=More}};

idle({stream, CommandBin, Fun}, _From, #state{conn=Sphero,buff=Buffer}=State) ->
    Sphero#sphero_bt.serial ! {send, CommandBin},
    CommandResponse = response(Buffer),
    {message, MRSP, _, Data, More} = CommandResponse,
    case MRSP of
        8 -> Fun({stream,8,Data});
        _ -> ok
    end,
    {reply, CommandResponse, idle, State#state{stream_cb=Fun,buff=More}}.

idle(disconnect, State) ->
    {stop, normal, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info({data, Packet}, StateName, #state{buff=Buffer,stream_cb=Fun}=State) ->
    Assemble = << Buffer/binary, Packet/binary >>,
    case decode(Assemble) of
        {more, _Data} ->
            {next_state, StateName, State#state{buff=Assemble}};
        {async, IdCode, Data, More} -> 
            case is_function(Fun) of
                true -> Fun({stream,IdCode,Data});
                false -> do_nothing
            end,
            {next_state, StateName, State#state{buff=More}}
    end.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

%% internal

response(Frag) ->
    receive
        {data, Data} -> 
            Assemble = << Frag/binary , Data/binary >>,
            case Assemble of
            << 16#FF:8, _SOP2:8, MRSP:8, SEQ:8, 1:8, _CkSum:8, Rest/binary>> ->
               {message, MRSP, SEQ, <<>>, Rest};
            << 16#FF:8, _SOP2:8, MRSP:8, SEQ:8, DataLen:8, Rest/binary>> ->
            case byte_size(Rest) >= DataLen of
                true ->
                    PayloadSize = (DataLen),
                    DataSize = PayloadSize-1,
                    MoreSize = (byte_size(Rest)) - PayloadSize,
                    << Payload:PayloadSize/binary, More:MoreSize/binary >> = Rest,
                    << Data2:DataSize/binary, _Cksum:8 >> = Payload,
                    {message, MRSP, SEQ, Data2, More};
                false -> response(Assemble)
            end;
            Other -> response(Other)
            end
    end.

decode(Packet) ->
  case Packet of
    << 16#FF:8, 16#FE:8, IdCode:8, DataLen:16, Rest/binary>> ->
        case byte_size(Rest) >= DataLen of
            true ->
                PayloadSize = (DataLen),
                DataSize = PayloadSize-1,
                MoreSize = (byte_size(Rest)) - PayloadSize,
                << Payload:PayloadSize/binary, More:MoreSize/binary >> = Rest,
                << Data2:DataSize/binary, _Cksum:8 >> = Payload,
                {async, IdCode, Data2, More};
            false -> {more, Packet}
        end;
    _ -> {more, Packet}
  end.

