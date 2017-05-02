import os
import serial
import time

def getcts(ser):
	ser.write("getcts\n")
	time.sleep(1)
	print ser.read(1)
	return

os.system("stty -F /dev/ttyS0 crtscts")

ser=serial.Serial("/dev/ttyS0",115200)
ser.dsrdtr = True
ser.rtscts = False


while True:
	ser.setDTR(False)
	time.sleep(0.5)	
	print ser.getDSR()

ser.setRTS(True)
getcts(ser)

ser.setRTS(False)
getcts(ser)

ser.setRTS(True)
getcts(ser)

ser.write("rtson\n")
time.sleep(1)
print ser.getCTS()

ser.write("rtsoff\n")
time.sleep(1)
print ser.getCTS()

ser.write("rtson\n")
time.sleep(1)
print ser.getCTS()
