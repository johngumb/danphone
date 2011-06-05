import sys
import os
import time
#import serial

import ft232r
import TC9181F

#D0 - DATA
#D1 - CLK
#D2 - STB RX
#D3 - STB TX
#D4 - LD RX
#D5 - LD TX
#D6 - RX DATA
#D7 - TX en

delay=0.0001

def outbit(hw,valn):

    clk=False

    if valn==0:
        val=False
    else:
        val=True

    print valn

    hw.setboolbit(hw.D0,val)

    hw.setboolbit(hw.D1,False)

    time.sleep(delay)

    hw.setboolbit(hw.D1,True)

    time.sleep(delay)

    hw.setboolbit(hw.D1,False)

    return

def latch(s):

    s.setboolbit(s.D3,False)

    time.sleep(delay)

    s.setboolbit(s.D3,True)

    time.sleep(delay)

    s.setboolbit(s.D3,False)

    return

def enable_outputs(s0,s1):

    d=[1,0,1,1]
    for x in d:
        outbit(hw,x)

    latch(hw)

    return

if __name__=="__main__":

    os.system("lsmod | grep -q ftdi_sio && rmmod ftdi_sio")
    #os.system("/usr/bin/chrt -r -p 99 %s"%os.getpid())

    hwif=ft232r.ft232r()

    txsynth = TC9181F.TC9181F(hwif,\
                              DATA=hwif.D0,\
                              CLK=hwif.D1,\
                              STB=hwif.D3,\
                              LOCK=hwif.D5)

    rxsynth = TC9181F.TC9181F(hwif,\
                              DATA=hwif.D0,\
                              CLK=hwif.D1,\
                              STB=hwif.D2,\
                              LOCK=hwif.D4)

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

    txsynth.set_ref_divider_bitlist(r)

    print

#    enable_outputs(hwif)

    #d=[1,0,1,1]
    #for x in d:
    #    outbit(hwif,x)

    #latch(s0)

    # set up counter
#    a = [1,1,0,0,0,1,1]
    # 91.2 MHz minimum.
    # 82.8 MHz minimum.
    #a_min = [0,0,0,0,0,0,0]

    a = [1,1,1,1,0,0,0]
    a = [0,0,0,0,0,0,0]
    a = [0,0,1,1,1,1,0]
    a = [1,1,1,0,0,0,1]


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
        outbit(hwif,x)

    print

    latch(hwif)

    print


    # phase comparator
    d = [0,0]

    c = [0,0]
    for x in d + c:
        outbit(hwif,x)

    latch(hwif)

    hwif.bb.port|=0x40
    time.sleep(0.001)
    hwif.bb.port&=~0x40

    for i in [1,2,3,4,5,6]:
        hwif.bb.ftdi_usb_purge_rx_buffer()
    
        print hwif.bb.port & 0x30
        time.sleep(1)
