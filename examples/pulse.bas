10 X = 0
20 backLED X
30 delay 1
40 X = X + 1
50 Y = 255-X
50 RGB Y,Y,Y
50 if X = 256 then goto 10
60 goto 20
