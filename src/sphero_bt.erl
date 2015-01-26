-module(sphero_bt).

-export([connect/4]).
-export([disconnect/1]).

-include("src/sphero.hrl").

-spec connect(tuple(),non_neg_integer(),string(),pos_integer()) -> sphero_bt().
%% @doc Connect to Orbotix Sphero
connect(BtDevice,BtChannel,CommPort,CommBaud) ->
    %% Force a BT connection. This assumes the Orbotix Sphero has been paired
    %% and is not asleep. Shake to wake and make sure it is in range and that
    %% your bluetooth device is active and online.
    %%
    {ok,R} = rfcomm:open(BtDevice,BtChannel),

    %% We're not going to use RFCOMM, once we've established a connection
    %% we can drop to a serial connection. Yes, This is a bit of a hack...
    rfcomm:close(R),

    %% Use a serial connection to open a communications channel with the sphero
    Serial = serial:start([{speed,CommBaud},{open,CommPort}]),

    #sphero_bt{ 
        bt_device = BtDevice,
        bt_channel = BtChannel,
        comm_port = CommPort,
        comm_baud = CommBaud,
        serial = Serial
    }.

-spec disconnect(sphero_bt()) -> ok.
%% @doc Disconnect from Orbotix Sphero
disconnect(#sphero_bt{serial=Serial}) ->
    Serial ! {disconnect},
    wait_for_death(Serial),
    ok.

wait_for_death(Pid) ->
    case is_process_alive(Pid) of
        true -> 
            erlang:yield(),
            wait_for_death(Pid);
        false ->
            ok
    end.
