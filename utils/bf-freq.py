#https://stackoverflow.com/questions/2648151/python-frequency-detection
# Read in a WAV and find the freq's
import pyaudio
import wave
import numpy as np

chunk = 2048*4

# open up a wave
#wf = wave.open('test-tones/440hz.wav', 'rb')
#wf = wave.open('ft8_t2_sox.wav', 'rb')
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
                 input_device_index=0,
                format=pyaudio.paInt16,
                frames_per_buffer = chunk,
                 rate = RATE)

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
        print "The freq is %f Hz." % (thefreq)
    else:
        thefreq = which*RATE/chunk
        print "The freq is %f Hz." % (thefreq)
    # read some more data
    data = stream.read(chunk)
if data:
    stream.write(data)
stream.close()
p.terminate()
