import sys
import socket
import time
import subprocess
import os
import string

cq_syms=[2, 5, 6, 0, 4, 1, 3, 5, 7, 3, 0, 0, 0, 3, 3, 6, 2, 7, 4, 3, 4, 0, 1, 1, 2, 2, 2, 1, 5, 6, 3, 2, 3, 1, 2, 0, 2, 5, 6, 0, 4, 1, 3, 7, 6, 4, 0, 4, 0, 6, 1, 4, 7, 6, 6, 2, 7, 2, 6, 0, 2, 0, 3, 7, 7, 2, 5, 0, 6, 0, 1, 6, 2, 5, 6, 0, 4, 1, 3]
test_syms=[2, 5, 6, 0, 4, 1, 3, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 2, 5, 6, 0, 4, 1, 3, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 2, 5, 6, 0, 4, 1, 3]
reply_syms=[2, 5, 6, 0, 4, 1, 3, 3, 4, 6, 5, 0, 1, 3, 7, 2, 1, 7, 1, 6, 4, 6, 6, 1, 0, 6, 5, 2, 5, 2, 0, 7, 6, 0, 7, 2, 2, 5, 6, 0, 4, 1, 3, 4, 0, 0, 5, 6, 1, 3, 7, 3, 7, 6, 6, 2, 7, 2, 6, 0, 2, 0, 3, 7, 7, 2, 5, 0, 2, 1, 6, 2, 2, 5, 6, 0, 4, 1, 3]


#{0: 1500.0,
# 1: 1506.25,
# 2: 1512.5,
# 3: 1518.75,
# 4: 1525.0,
# 5: 1531.25,
# 6: 1537.5,
# 7: 1543.75}

g_tones={}
old_freq_offset=0.0
old_twice_dac_offset=0.0

def init_tones(basefreq):
    for i in range(8):
        g_tones[i]=i*6.25 + basefreq

def freq_to_dac(freq):
    global old_freq_offset
    global old_twice_dac_offset

    base_f=1500
    #base_dac=2611
    #base_dac=2601
    base_dac=2605
    twice_base_dac=base_dac*2

    hz_per_count = 1.046
    #hz_per_count = 1.046
    freq_offset=freq-base_f

    diff_offset = freq_offset - old_freq_offset

    sqf = diff_offset/32
    print sqf
    if diff_offset>=0:
        square_offset = sqf*sqf
    else:
        square_offset = -1*sqf*sqf

    square_offset = 0
    print square_offset
    # 4.4 and 16 look ok
    twice_dac_offset=(2*(freq_offset+(diff_offset/4.2)+square_offset))/hz_per_count
    #twice_dac_offset=(2*freq_offset)/hz_per_count

    #print twice_dac_offset - old_twice_dac_offset

    if False:
        boundary=10
        fudge=5
        if diff_offset > 0:
            if (twice_dac_offset - old_twice_dac_offset) < boundary:
                twice_dac_offset += fudge
        else:
            if (old_twice_dac_offset - twice_dac_offset) < boundary:
                twice_dac_offset -= fudge
            
    twice_dac_offset_int = int(round(twice_dac_offset))

    twice_dac_val = twice_dac_offset_int + twice_base_dac
    
    if twice_dac_val % 2 == 0:
        dac_val = twice_dac_val/2
        dac_cmd = 2
    else:
        dac_val = (twice_dac_val-1)/2
        dac_cmd = 4

    result = (dac_cmd, dac_val)
    #print result

    old_freq_offset = freq_offset
    old_twice_dac_offset = twice_dac_offset
    
    return result
    
def sym_to_dac(tone_num):
    freq = g_tones[tone_num]
    #print freq
    return freq_to_dac(freq)

def send_dac(val):
    cmd = "D%d%X" % val
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

    print len(test_syms)
    test_syms2 = [ x if x!=8 else 7 for x in test_syms ]
    test_syms3 = [ x if x!=9 else 0 for x in test_syms2 ]
    print test_syms3

    zero_rx = "D2C3E" # for rx
    zero = "D2A2D"

    sim = len(sys.argv) > 1 and sys.argv[1] != "p"

    if not sim:
        send_msg("ft8-txon")
        send_msg(zero)

        recfile = '/home/john/ft8_t2.wav'
        recfile_final = '/home/john/ft8_t2_sox.wav'

        p = subprocess.Popen(['jack_capture', '-as', '--port', 'sdr_rx:ol', recfile ])        
        # time to adjust to near tx freq
        time.sleep(2)
    
        #freq_to_dac(g_tones[5])


        #f=open("g4rdc-cq-tones")
        #cq_syms = [ int(x.strip()) for x in f.readlines() ]
        #f.close()

        # last thing we do before sending message
        if len(sys.argv) > 1:
            send_msg("pa-on")

        time.sleep(0.1)
    
    #for i in test_syms3:
    lastsym = -1
    for i in cq_syms:
#    for i in reply_syms:
        d = sym_to_dac(i)
        if sim:
            (c, v) = d
            if c==4:
                print v-0.5
            else:
                print v
        else:
            if d != lastsym:
                send_dac(d)
                #time.sleep(0.159)
                time.sleep(0.158)
            else:
                time.sleep(0.161)

        lastsym = d

    if not sim:
        send_msg("ft8-txoff") # disables PA

        time.sleep(1)

        p.terminate()

        status = p.communicate()
        print status

        retcode = p.wait()
        print retcode

        send_msg(zero_rx)
        os.system(string.join(['sox', recfile, '-t wavpcm --rate 12k -c 1 -b 16', recfile_final, '&&', 'rm -f', recfile]))

        os.system(string.join(['python', '/home/john/basicft8/basicft8.py', '/home/john/ft8_t2_sox.wav', '>', '/home/john/basicft8/x']))


        #send_msg("Q90")
        #send_dac(1529)
