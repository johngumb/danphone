#https://stackoverflow.com/questions/2648151/python-frequency-detection
# Read in a WAV and find the freq's
import pyaudio
import wave
import numpy as np
import time
import socket
import os

chunk = 2048*256

def send_msg(msg):
    s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    s.connect("/tmp/mui-ext.s.6m")
    s.send(msg)
    s.close()

def send_dac(val):
    cmd = "D%d%03X" % val
    print cmd
    send_msg(cmd)

# open up a wave
#wf = wave.open('test-tones/440hz.wav', 'rb')
wf = wave.open('ft8_t2_sox.wav', 'rb')
#swidth = wf.getsampwidth()
swidth=2
#RATE = wf.getframerate()
RATE = 48000

# use a Blackman window
window = np.blackman(chunk)
# open stream
p = pyaudio.PyAudio()
stream = p.open(
                channels = 1,
                input = True,
                output = False,
                 input_device_index=9,
                format=pyaudio.paInt16,
                frames_per_buffer = chunk,
                 rate = RATE)

ofile="dacdata.csv"
os.unlink(ofile)

i=0
send_dac((2,0))
time.sleep(10)
# read some data
data = stream.read(chunk)
# play stream and find the frequency of each chunk
while True:
    # write data out to the audio stream
    #stream.write(data)
    # unpack the data and times by the hamming window
    indata = np.array(wave.struct.unpack("%dh"%(len(data)/swidth),\
                                         data))*window
    # Take the fft and square each value
    fftData=abs(np.fft.rfft(indata))**2
    # find the maximum
    which = fftData[1:].argmax() + 1
    # use quadratic interpolation around the max
    if which != len(fftData)-1:
        y0,y1,y2 = np.log(fftData[which-1:which+2:])
        x1 = (y2 - y0) * .5 / (2 * y1 - y2 - y0)
        # find the frequency and output it
        thefreq = (which+x1)*RATE/chunk
        print "%d The freq is %f Hz." % (i, thefreq)
    else:
        thefreq = which*RATE/chunk
        print "%d The freq is %f Hz." % (i, thefreq)

    a=open(ofile,"a+")
    a.write("%d,%f\n"% (i, thefreq))
    a.close()
    # read some more data
    data = stream.read(chunk)

    send_dac((2,i))
    time.sleep(10)
    i+=1
    if i==4096:
        break
stream.close()
p.terminate()