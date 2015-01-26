**sphero-erl** -- Erlang Orbotix Sphero Interface
=====

# Status

Experimental

# About

This is a work in progress library implementing the Orbotix API for the
Sphero, Sphero 2 and Ollie devices. The API supports direct manipulation
of the device via an Erlang API, streaming sensor data from the device to
Erlang, and injecting macro and Orb Basic code onto the device. 

====

# Examples

Examples are provided in the examples folder:

* **breathe.erl** - Simulate breathing like a pre unibody Apple MacBookPro using the main Sphero LED
* **accel.erl** - Stream accelerometer data
* **gyro.erl** - Stream gyro data
* **square.erl** - Move sphero in a square
* **ob.erl** - Inject and run Orb basic on the sphero
  * double_shake.bas - Detect a double shake gesture
  * police.bas - Flash red and blue like a police car
  * pulse.bas - Pulse the primary and heading LEDs
  * rgb.bas - Repeating Red, Green, Blue color sequence
  * spin.bas - Spin the sphero

**Running the samples**

Firstly, configure your Sphero's bluetooth id and serial connection details:

```
%% file: sphero.config
{sphero, [
  {id,{0,6,102,75,184,135}},                %% Bluetooth id (hex->decimal)
  {channel,1},                              %% RFCOMM Channel
  {serial,"/dev/tty.Sphero-RPP-RN-SPP"},    %% Serial protocol profile
  {baud,115200}                             %% Baud rate
]}.

```

Most examples require no arguments

```
$ ERL_LIBS=deps erl escript examples/breathe.erl
$ ERL_LIBS=deps erl escript examples/accel.erl
$ ERL_LIBS=deps erl escript examples/gyro.erl
$ ERL_LIBS=deps erl escript examples/square.erl

```

The Orb Basic command can execute a script

```
$ ERL_LIBS=deps erl escript examples/ob.erl -a exec <path/to/script.bas>
```

or it can stop an executing script

```
$ ERL_LIBS=deps erl -pa ebin escript examples/ob.erl -a exec <path/to/script.bas>
```

It is possible to mix and match Orb Basic and other scripts. For example,
install the police.bas Orb Basic script, then running the square.erl script.

# References

The API in its current form is minimal. The following references are useful
guides and were the primary sources used to flesh out this API:

* (Sphero API)[http://orbotixinc.github.io/Sphero-Docs/docs/sphero-api/index.html]
* (Sphero Orb Basic)[https://s3.amazonaws.com/docs.gosphero.com/api/Sphero_orbBasic.pdf]
* (Sphero Macros)[https://s3.amazonaws.com/docs.gosphero.com/api/Sphero_Macros.pdf]
* (Sphero Shell)[https://s3.amazonaws.com/docs.gosphero.com/api/Sphero_Shell_Commands_1.0.pdf]

====

Enjoy!
