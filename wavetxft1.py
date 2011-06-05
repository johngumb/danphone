import sys
import os
import time
#import serial

import ft232r

#D0 - DATA
#D1 - CLK
#D2 - STB RX
#D3 - STB TX
#D4 - LD RX
#D5 - LD TX
#D6 - RX DATA
#D7 - TX en

#DTR0 - blue DATA
#RTS0 - purple STB RX
#DTR1 - (blue on 232 side) then green CLK
#CTS0 - black LD
#RTS1 - yellow STB TX

delay=0.0001

def outbit(hw,valn):

    clk=True
    if valn==0:
        val=False
    else:
        val=True

    print valn

    hw.setboolbit(hw.D0,val)

    hw.setboolbit(hw.D1,not clk)

    time.sleep(delay)

    hw.setboolbit(hw.D1,clk)

    time.sleep(delay)

    hw.setboolbit(hw.D1,not clk)

    return

def latch(s):

    val=True

    s.setboolbit(s.D3,not val)

    time.sleep(delay)

    s.setboolbit(s.D3,val)

    time.sleep(delay)

    s.setboolbit(s.D3,not val)

    return

def enable_outputs(s0,s1):

    d=[1,0,1,1]
    for x in d:
        outbit(hw,x)

    latch(hw)

    return

if __name__=="__main__":

    os.system("lsmod | grep -q ftdi_sio && rmmod ftdi_sio")
    os.system("/usr/bin/chrt -r -p 99 %s"%os.getpid())

    hw=ft232r.ft232r()

    # set up reference divider
#    r=[1,0,0,0,0,0,0,0,0,0,0,1]
    r=[1,1,1,1,1,1,1,1,1,1,1,0]
    r=[0,0,1,1,1,1,0,0,0,0,1,0] # good
    r=[1,1,1,1,1,1,1,0,0,0,1,0] # good 1 jan
    r=[1,0,1,1,1,1,0,0,0,0,1,0] # good    
    r=[0,0,0,0,0,0,1,0,0,0,1,0] # good
    r=[0,0,0,0,0,0,1,0,0,0,1,0] # good 17-apr
#    r=[0,0,0,0,0,0,0,0,0,1,0,0]

#    r=[0,0,0,0,0,0,0,0,0,0,1,0]
#    r=[1,1,1,1,1,1,1,1,1,1,1,1]
#    r=[1,1,1,1,1,1,1,1,1,1,1,1]

    # TODO figure out what L2 is - depends whether LPF is inverting or non
    # inverting
    l=[0,0]
    c=[0,1]

#    s0.setRTS(True)

    for x in r+l+c:
        outbit(hw,x)

    print

    latch(hw)

    print

#    enable_outputs(hw)

    #d=[1,0,1,1]
    #for x in d:
    #    outbit(s0,s1,x)

    #latch(s0)

    # set up counter
#    a = [1,1,0,0,0,1,1]
    # 91.2 MHz minimum.
    # 82.8 MHz minimum.
    #a_min = [0,0,0,0,0,0,0]

    a = [1,1,1,1,0,0,0]
    a = [0,0,0,0,0,0,0]
    a = [0,0,1,1,1,1,0]


    n = [0,1,0,1,1,1,1,1,0,0,0]
    n = [1,1,1,1,1,1,1,1,1,0,0]
    n = [0,0,0,0,0,0,0,0,0,0,1]
    n = [0,1,0,1,0,0,1,1,0,0,0]
    n = [0,0,0,0,0,0,1,1,0,0,0]
    n = [1,0,0,1,1,1,0,1,0,0,0] #17apr
    n = [1,0,0,1,1,1,0,1,0,0,0]    

#    n = [1,1,1,1,1,1,1,1,1,1,1]


    c = [1,0]
    for x in a + n + c:
        outbit(hw,x)

    print

    latch(hw)

    print


    # phase comparator
    d = [0,0]

    c = [0,0]
    for x in d + c:
        outbit(hw,x)

    latch(hw)

    for i in [1,2,3,4,5,6]:
        print hw.bb.port & 0x30
        time.sleep(1)
