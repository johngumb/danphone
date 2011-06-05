import serial
import time
#DTR0 - blue DATA
#RTS0 - purple STB
#DTR1 - green CLK
#CTS0 - black LD

delay=0.001

def getserials():
    s0 = serial.Serial("/dev/ttyUSB0")

    s1 = serial.Serial("/dev/ttyUSB1")

    return (s0,s1)

def test():
    period=0.1

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
    clk=False
    clk=True
    if valn==0:
        val=False
    else:
        val=True

    print valn
    s0.setDTR(val)
    s1.setDTR(clk)
    time.sleep(delay)
    s1.setDTR(not clk)
    time.sleep(delay)

    return

def latch(s0):
    val=True
    s0.setRTS(val)
    time.sleep(delay)
    s0.setRTS(not val)
    time.sleep(delay)
    s0.setRTS(val)
    return

if __name__=="__main__":

    d=[1,0,0,0,0,1,0,1,0,1,0,1]
    l=[1,0]
    c=[0,1]
    (s0,s1)=getserials()
    s0.setRTS(True)

    for x in d+l+c:
        outbit(s0,s1,x)

    latch(s0)

    d=[1,0,1,1]
    for x in d:
        outbit(s0,s1,x)

    latch(s0)

    d = [1,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0]
    c = [1,0]
    for x in d + c:
        outbit(s0,s1,x)
	
    latch(s0)

    #while True:
    #    print s0.getCTS()
    #    time.sleep(1)

    #test()
