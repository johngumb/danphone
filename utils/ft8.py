import socket
import time

g_tones={}

cq_tones=[2, 5, 6, 0, 4, 1, 3, 5, 7, 3, 0, 0, 0, 3, 3, 6, 2, 7, 4, 3, 4, 0, 1, 1, 2, 2, 2, 1, 5, 6, 3, 2, 3, 1, 2, 0, 2, 5, 6, 0, 4, 1, 3, 7, 6, 4, 0, 4, 0, 6, 1, 4, 7, 6, 6, 2, 7, 2, 6, 0, 2, 0, 3, 7, 7, 2, 5, 0, 6, 0, 1, 6, 2, 5, 6, 0, 4, 1, 3, 0]

def init_tones(basefreq):
    for i in range(8):
        g_tones[i]=i*6.25 + basefreq

def freq_to_dac(freq):
    base_f=1500
    base_dac=2611
    
    freq_offset=freq-base_f
    dac_offset=freq_offset/1.046

    return int(dac_offset) + base_dac
    
def tone_to_dac(tone_num):
    freq = g_tones[tone_num]
    return freq_to_dac(freq)

def send_dac(val):
    cmd = "D2%X" % val
    print cmd
    send_msg(cmd)

def send_msg(msg):
    s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    s.connect("/tmp/mui-ext.s.6m")
    s.send(msg)
    s.close()
    
if __name__ == "__main__":
    init_tones(1500)
    print g_tones

    freq_to_dac(g_tones[5])


    #f=open("g4rdc-cq-tones")
    #cq_tones = [ int(x.strip()) for x in f.readlines() ]
    #f.close()
    #print cq_tones
    
    for i in cq_tones:
        send_dac(tone_to_dac(i))
        time.sleep(0.158)
#        time.sleep(0.157)
    
    #send_msg("D2C2E")
    #send_msg("Q90")
    #send_dac(1529)
