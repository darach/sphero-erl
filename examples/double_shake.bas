10 if dshake > 0 then goto 40 else LEDC 2
20 delay 100
30 goto 10
40 LEDC 1
50 delay 1000
60 goto 10
