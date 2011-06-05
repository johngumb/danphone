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

if __name__=="__main__":

    os.system("lsmod | grep -q ftdi_sio && rmmod ftdi_sio")
    #os.system("/usr/bin/chrt -r -p 99 %s"%os.getpid())

    hwif=ft232r.ft232r()

    # enable transmitter

    hwif.bb.port &= ~hwif.D7

    time.sleep(0.5)

    hwif.bb.port |= hwif.D7

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

    refclk = 12.8E6
    rxsynth.set_refclk(refclk)
    txsynth.set_refclk(refclk)

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

#    txsynth.set_ref_divider_bitlist(r)
    txsynth.set_ref_divider(1088)

    print

    # set up counter
#    a = [1,1,0,0,0,1,1]
    # 91.2 MHz minimum.
    # 82.8 MHz minimum.
    #a_min = [0,0,0,0,0,0,0]

    a = [1,1,1,1,0,0,0]
    a = [0,0,0,0,0,0,0]
    a = [0,0,1,1,1,1,0]
#    a = [1,1,1,1,0,0,1]


    n = [0,1,0,1,1,1,1,1,0,0,0]
    n = [1,1,1,1,1,1,1,1,1,0,0]
    n = [0,0,0,0,0,0,0,0,0,0,1]
    n = [0,1,0,1,0,0,1,1,0,0,0]
    n = [0,0,0,0,0,0,1,1,0,0,0]
    n = [1,0,0,1,1,1,0,1,0,0,0] #17apr
    n = [1,0,0,1,1,1,0,1,0,0,0]    

    n = 185

#    n = [1,1,1,1,1,1,1,1,1,1,1]

#    txsynth.set_counter_bitlist(a,n)
#    txsynth.set_counter(23740)
#    txsynth.set_counter(23836)

    txsynth.set_freq(70.4E6)

    print

    txsynth.enable_phase_comparator()

    hwif.bb.port|=0x40
    time.sleep(0.001)
    hwif.bb.port&=~0x40

    for i in [1,2,3,4,5,6]:
        #hwif.bb.ftdi_usb_purge_rx_buffer()
        hwif.bb.ftdi_fn.ftdi_usb_purge_rx_buffer()
        print hwif.bb.port & 0x30
        time.sleep(1)
