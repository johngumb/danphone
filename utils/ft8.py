import sys
import socket
import time
import subprocess
import os
import string

cq_syms=[2, 5, 6, 0, 4, 1, 3, 5, 7, 3, 0, 0, 0, 3, 3, 6, 2, 7, 4, 3, 4, 0, 1, 1, 2, 2, 2, 1, 5, 6, 3, 2, 3, 1, 2, 0, 2, 5, 6, 0, 4, 1, 3, 7, 6, 4, 0, 4, 0, 6, 1, 4, 7, 6, 6, 2, 7, 2, 6, 0, 2, 0, 3, 7, 7, 2, 5, 0, 6, 0, 1, 6, 2, 5, 6, 0, 4, 1, 3]
test_syms=[2, 5, 6, 0, 4, 1, 3, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 2, 5, 6, 0, 4, 1, 3, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 2, 5, 6, 0, 4, 1, 3]
reply_syms=[2, 5, 6, 0, 4, 1, 3, 3, 4, 6, 5, 0, 1, 3, 7, 2, 1, 7, 1, 6, 4, 6, 6, 1, 0, 6, 5, 2, 5, 2, 0, 7, 6, 0, 7, 2, 2, 5, 6, 0, 4, 1, 3, 4, 0, 0, 5, 6, 1, 3, 7, 3, 7, 6, 6, 2, 7, 2, 6, 0, 2, 0, 3, 7, 7, 2, 5, 0, 2, 1, 6, 2, 2, 5, 6, 0, 4, 1, 3]

g_server =  None
g_last_freq =0.0
g_last_sym = 8
g_last_result = None

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

def freq_to_dac(sym, freq):
    global old_freq_offset
    global old_twice_dac_offset
    global g_last_freq
    global g_last_result
    global g_last_sym

    neutral=0xC3E # 50.315, 2kHz above 50.313
    #hz_per_count = 1.058
    #hz_per_count = 1.046
    #hz_per_count = 1.059
    if freq>=1400:
        hz_per_count = 1.0585
    else:
        hz_per_count = 1.0585

    # if freq < 1100:
    #     factor = 0.985
    # elif freq < 1400:
    #     factor = 0.995
    # else:
    #     factor = 1.00

    if freq < 1400:
        factor = 1.0 - 0.05*((2000.0-freq)/2000.0)
    else:
        factor = 1.0
    print "factor", factor

    dac=neutral - ((2000-freq) * hz_per_count * factor)

    twice_dac = 2 * dac

    if g_last_freq:
        diff_offset = freq - g_last_freq
    else:
        diff_offset = 0

    #17 nov twice_dac_offset=(2*(freq_offset+(diff_offset/4.2)+square_offset))/hz_per_count

    #4.17 looks good 17 nov
    if diff_offset>0:
        factor=4.17
    else:
        factor=4.17

    twice_dac_offset=(2*diff_offset/factor)/hz_per_count

    if sym > 4 and g_last_sym >= 4:
        twice_dac_offset += 2

    twice_dac = int(round(twice_dac))

    twice_dac_val = twice_dac + twice_dac_offset
    
    if twice_dac_val % 2 == 0:
        dac_val = twice_dac_val/2
        dac_cmd = 2
    else:
        dac_val = (twice_dac_val-1)/2
        dac_cmd = 4

    result = (dac_cmd, dac_val)
    g_last_result = result

    #print result

    g_last_freq = freq
    g_last_sym = sym

    return result
    
def sym_to_dac(tone_num):
    freq = g_tones[tone_num]
    #print freq
    return freq_to_dac(tone_num,freq)

def send_dac(val):
    cmd = "D%d%X" % val
    print cmd
    send_msg(cmd)

def send_msg(msg):
    g_server.listen(1)

    s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    s.connect("/tmp/mui-ext.s.6m")
    s.send(msg)
    s.close()

    conn, addr = g_server.accept()
    datagram = conn.recv(1024)
    assert(datagram==msg)
    #print "got response",datagram

def setup_response_socket(Socket):
    global g_server

    sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    if os.path.exists(Socket):
        os.remove(Socket)

    g_server = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    g_server.bind(Socket)
    
if __name__ == "__main__":
    base_f=1803
    init_tones(base_f)
    print g_tones

    response_socket = "/tmp/ft8response"

    setup_response_socket(response_socket)
    
    print len(test_syms)
    test_syms2 = [ x if x!=8 else 7 for x in test_syms ]
    test_syms3 = [ x if x!=9 else 0 for x in test_syms2 ]
    print test_syms3

    zero_rx = "D2C3E" # for rx
    #zero = "D2A2D"
    zero = freq_to_dac(0, base_f)
    
    sim = len(sys.argv) > 1 and sys.argv[1] != "p"

    if not sim:
        send_msg("ft8-txon")
        send_dac(zero)

        recfile = '/home/john/ft8_t2.wav'
        recfile_final = '/home/john/ft8_t2_sox.wav'

        # time to adjust to near tx freq
        time.sleep(2)



        #freq_to_dac(g_tones[5])


        #f=open("g4rdc-cq-tones")
        #cq_syms = [ int(x.strip()) for x in f.readlines() ]
        #f.close()

        # last thing we do before sending message
        if len(sys.argv) > 1:
            send_msg("pa-on")

        # synch on messages required - send synch delay eventually
        #send_msg("E9F88")
        #send_msg("EA198") #320ms
        #send_msg("EA1A0")  #320.06ms
        send_msg("EA19F")
        p = subprocess.Popen(['jack_capture', '-as', '--port', 'sdr_rx:ol', recfile ])

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
            print i,
            n=time.time()
            send_dac(d)
            print time.time()-n

        lastsym = d

    if not sim:
        send_msg("ft8-txoff") # disables PA
        send_msg("E0000")        
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

    os.unlink(response_socket)
