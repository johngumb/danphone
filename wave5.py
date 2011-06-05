import sys
import os
import time
import serial

#DTR0 - blue DATA
#RTS0 - purple STB RX
#DTR1 - (blue on 232 side) then green CLK
#CTS0 - black LD
#RTS1 - purple STB TX

delay=0.001

def getserials():
    s0 = serial.Serial("/dev/ttyUSB0")

    s1 = serial.Serial("/dev/ttyUSB1")

    return (s0,s1)

def test():
    period=0.001

    i = 0
    s=serial.Serial("/dev/ttyUSB1")
    while True:
        s.setDTR(True)
        #s.setRTS(True)
        time.sleep(period)
        
        i = i + 1

	if i % 10 == 0:
		print s.getCTS()

        s.setDTR(False)
        #s.setRTS(False)
        time.sleep(period)

def outbit(s0,s1,valn):
    clk=True
    if valn==0:
        val=True
    else:
        val=False

    #print valn
    s0.setDTR(val) # rx strobe
    time.sleep(delay/10)
    s1.setDTR(clk)
    time.sleep(delay)
    s1.setDTR(not clk)
    time.sleep(delay)

    return

def latch(s0):
    val=True

    s1.setRTS(val) # tx strobe
    s0.setRTS(val)
    time.sleep(delay)
    s1.setRTS(not val) # tx strobe
    s0.setRTS(not val)
    time.sleep(delay)
    s0.setRTS(val)
    s1.setRTS(val) # tx strobe
    return

def enable_outputs(s0,s1):

    d=[1,0,1,1]
    for x in d:
        outbit(s0,s1,x)

    latch(s0)

    return

if __name__=="__main__":

    os.system("/usr/bin/chrt -r -p 99 %s"%os.getpid())

    (s0,s1)=getserials()

    # set up reference divider
#    r=[1,0,0,0,0,0,0,0,0,0,0,1]
    r=[1,1,1,1,1,1,1,1,1,1,1,0]
    r=[0,0,1,1,1,1,0,0,0,0,1,0] # good
    r=[1,1,1,1,1,1,1,0,0,0,1,0] # good 1 jan
    r=[1,0,1,1,1,1,0,0,0,0,1,0] # good    
    r=[0,0,0,0,0,0,1,0,0,0,1,0] # good
    r=[0,0,0,0,0,0,0,0,1,0,1,0] # good 10kHz steps
    r=[0,0,0,0,0,0,0,0,0,0,0,1]
#    r=[0,0,0,0,0,0,0,0,0,1,0,0]

#    r=[0,0,0,0,0,0,0,0,0,0,1,0]
#    r=[1,1,1,1,1,1,1,1,1,1,1,1]
#    r=[1,1,1,1,1,1,1,1,1,1,1,1]

    # TODO figure out what L2 is - depends whether LPF is inverting or non
    # inverting
    l=[0,0]
    c=[0,1]

    s0.setRTS(True)

    for x in r+l+c:
        outbit(s0,s1,x)

    print

    latch(s0)

    print

#    enable_outputs(s0,s1)

    #d=[1,0,1,1]
    #for x in d:
    #    outbit(s0,s1,x)

    #latch(s0)

    # set up counter
#    a = [1,1,0,0,0,1,1]
    # 91.2 MHz minimum.
    #a_min = [0,0,0,0,0,0,0]

    n_min = [1,0,1,1,1,0,0,0,1,0,0] #369

    a = [0,1,1,1,1,0,1] #91.7875
    a = [1,0,1,1,1,0,1]

    n = [0,0,0,1,0,0,0,0,1,0,0]
    n = [1,1,1,0,0,1,0,0,1,0,0] #95.1MHz
    n = [0,0,0,1,0,0,1,1,1,0,0] #91.7875
#    n = [1,1,1,1,1,1,1,1,1,1,1]

    c = [1,0]

    a1=[1,0,1,1,1,0,1]
    a2=[0,1,1,1,1,0,1]
    a3=[1,1,1,1,1,0,1]

    #a1=[0,1,1,1,0,1,1]

    while False:
        for a in [a1,a2,a3]:
            for x in a + n + c:
                outbit(s0,s1,x)
            latch(s0)
            print
            time.sleep(2)
            
    while True:
        #i=36400
        i=37080
        #i=37588
        #i=37666
        #i=37838
        #i=38025
        i=37084
        i=36985
        #i=36934
        i=36442
        i=37102
        i=37104
        #i=60000
        limit=45400
        #limit=37630
        while i<limit:
            j=i
            print i
            for k in range(18):
                bv = j & 1
                outbit(s0,s1,bv)
                j = j >> 1

            outbit(s0,s1,1)
            outbit(s0,s1,0)
            time.sleep(2.5)
            latch(s0)
            i = i + 1


    # phase comparator
#    d = [0,0]

#    c = [0,0]
#    for x in d + c:
#        outbit(s0,s1,x)

#    latch(s0)

    #while True:
    #    print s0.getCTS()
    #    time.sleep(1)

    #test()
