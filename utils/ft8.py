import sys
import socket
import time
import subprocess
import os
import string
import csv
import math

cq_syms=[2, 5, 6, 0, 4, 1, 3, 5, 7, 3, 0, 0, 0, 3, 3, 6, 2, 7, 4, 3, 4, 0, 1, 1, 2, 2, 2, 1, 5, 6, 3, 2, 3, 1, 2, 0, 2, 5, 6, 0, 4, 1, 3, 7, 6, 4, 0, 4, 0, 6, 1, 4, 7, 6, 6, 2, 7, 2, 6, 0, 2, 0, 3, 7, 7, 2, 5, 0, 6, 0, 1, 6, 2, 5, 6, 0, 4, 1, 3]
test_syms=[2, 5, 6, 0, 4, 1, 3, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 2, 5, 6, 0, 4, 1, 3, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 2, 5, 6, 0, 4, 1, 3]
reply_syms=[2, 5, 6, 0, 4, 1, 3, 3, 4, 6, 5, 0, 1, 3, 7, 2, 1, 7, 1, 6, 4, 6, 6, 1, 0, 6, 5, 2, 5, 2, 0, 7, 6, 0, 7, 2, 2, 5, 6, 0, 4, 1, 3, 4, 0, 0, 5, 6, 1, 3, 7, 3, 7, 6, 6, 2, 7, 2, 6, 0, 2, 0, 3, 7, 7, 2, 5, 0, 2, 1, 6, 2, 2, 5, 6, 0, 4, 1, 3]

wsj2=[3, 1, 4, 0, 6, 5, 2, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 5, 5, 1, 7, 1, 0, 0, 6, 0, 7, 4, 0, 5, 4, 3, 0, 0, 7, 3, 1, 4, 0, 6, 5, 2, 1, 6, 7, 6, 7, 1, 2, 4, 6, 3, 6, 5, 5, 1, 6, 7, 0, 1, 4, 2, 6, 0, 6, 7, 0, 2, 7, 0, 0, 3, 1, 4, 0, 6, 5, 2, 0]

# wsj2 G3ZGZ G4RDC R-19
msg2=[3, 1, 4, 0, 6, 5, 2, 0, 3, 3, 0, 6, 7, 0, 6, 1, 0, 0, 5, 5, 1, 7, 1, 0, 0, 6, 2, 7, 4, 6, 0, 0, 2, 3, 0, 5, 3, 1, 4, 0, 6, 5, 2, 7, 4, 6, 3, 3, 4, 1, 4, 0, 1, 1, 7, 1, 2, 5, 4, 3, 2, 1, 1, 4, 3, 5, 3, 0, 2, 7, 0, 6, 3, 1, 4, 0, 6, 5, 2]

g_server =  None
g_last_freq =0.0
g_last_sym = 8
g_base_freq = None
g_base_dac = None
g_last_dac = None
g_local_count_per_hz = None
g_band = "6m"
g_fudge = 2.1

#{0: 1500.0,
# 1: 1506.25,
# 2: 1512.5,
# 3: 1518.75,
# 4: 1525.0,
# 5: 1531.25,
# 6: 1537.5,
# 7: 1543.75}

g_tones={}

def cal(calfreq):
    zero = freq_to_dac_max5216(0, calfreq, initial=True)
    send_dac(zero)
    send_msg("ft8-txon")
    time.sleep(20)
    send_msg("ft8-txoff")

def init_tones(basefreq):
    for i in range(8):
        g_tones[i]=i*6.25 + basefreq

def dv_to_f(dv):
    #[a, b, c, d] = [  1.74150917e-01  -4.43158994e+03   3.69915130e+04  -6.49876296e+03]
    f = (a * x) + (b/(x-c)) + d

def f_to_dv(F):
    global g_band
    #
    # from dacdata dacdata-2m-step1-opamp-linear.csv by curve fitting
    # using (a * x) + (b/(x-c)) + d
    #
    if g_band=="2m":
        [a, b, c, d] = [1.74150917e-01, -4.43158994e+03, 3.69915130e+04, -6.49876296e+03]

    if g_band=="4m":
        #[a, b, c, d] = [  8.94132636e-02, -1.34659984e+07, 3.74200375e+03, -2.08969879e+03]
        [a, b, c, d] = [9.10414222e-02, -7.04125513e+06, 1.11353326e+04, -2.27300009e+03]

    if g_band=="6m":
        #[a, b, c, d] = [5.53113481e-02, -7.48653896e+06,  7.24091607e+02, -6.11406818e+02]
        [a, b, c, d] = [5.40635806e-02, -1.37705324e+07, -6.69474575e+03, -4.57521898e+02]

    P=[a,b,c,d]
#    x=5000
#    y = testfunc(x,*P)
#    print(y)

    y=F
    A=a
    B=d - y - a * c
    C=b-(d*c)+(y*c)

    #test to prove we have correct quadratic parameters
    #print(A*(x**2) + B*x + C)

    #print (A,B,C)
    #print(math.sqrt(B**2 - 4*A*C))

    r = (-B + math.sqrt(B**2-(4*A*C)))/(2*A)

    return r

def freq_to_dac_max5216(sym, freq, initial=False):
    global g_last_freq
    global g_last_sym
    global g_base_freq
    global g_last_dac
    global g_fudge

    dac = int(f_to_dv(freq))

    if initial:
        return ("M","", dac)

    #
    # Equating a dac offset directly to a frequency
    # but a direct conversion i.e. factor of 1.0
    # seems fine.
    #
    if g_last_freq:
        dac_offset = (freq - g_last_freq) * g_fudge
    else:
        dac_offset = 0

#    dac_offset=0
    #print dac_offset
    #dac_offset *=2
    dac_val = dac + dac_offset

    if g_last_sym == sym:
        dac_val = g_last_dac

    result = ("M", "", dac_val)

    g_last_freq = freq
    g_last_sym = sym
    g_last_dac = dac_val

    return result
    
def sym_to_dac(tone_num):
    freq = g_tones[tone_num]
    #print freq
    return freq_to_dac_max5216(tone_num,freq)

def send_dac(val):
    cmd = "%s%s%X" % val
    print(cmd)
    send_msg(cmd)

def send_msg(msg):
    g_server.listen(1)

    s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    s.connect("/tmp/mui-ext.s.%s" % g_band)
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

def read_calfile(calfile):
    res = 0
    if os.path.exists(calfile):
        with open(calfile) as caldata:
            res = int(caldata.read())

    return res

def reset_dac():
    # HACK
    val=0xC3E
    val+=read_calfile("/home/john/6mcal")
    print "D2%X" % val
    send_msg("D2%X" % val)

def run_ft8(base_f):
    init_tones(base_f)
    print g_tones

    response_socket = "/tmp/ft8response"

    setup_response_socket(response_socket)

    if False:
        for f in (1000,1500,2000,2300):
            cal(f)
        reset_dac()

        sys.exit(0)

    print len(test_syms)
    test_syms2 = [ x if x!=8 else 5 for x in test_syms ]
    test_syms3 = [ x if x!=9 else 2 for x in test_syms2 ]
    print test_syms3

    #zero_rx = "D2C3E" # for rx
    zero_rx = "MC370"
    #zero = "D2A2D"
    zero = freq_to_dac_max5216(0, base_f, initial=True)
    
    sim = len(sys.argv) > 1 and sys.argv[1] != "p"

    if not sim:
        send_msg("ft8-txon")
        send_msg("EA010")  #160ms sync
        send_dac(zero)
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
        #send_msg("E9FF0")
        #send_msg("EA340")  #160ms sync
        #send_msg("EA280")  #160ms sync # working ok -24
        #send_msg("EA240")  #160ms sync # working ok -24
        #send_msg("EA320")  #160ms sync # working ok -24
        send_msg("EA200")

        p = subprocess.Popen(['jack_capture', '-as', '--port', 'sdr_rx:ol', recfile ])
    st=time.time()
#    for i in test_syms3:
#    for i in wsj2:
#    for i in reply_syms:
#    print len(msg2)
    for i in msg2:
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
    print "msg time",time.time() - st

    if not sim:
        send_msg("ft8-txoff") # disables PA
        send_msg("E0000") # stop 160ms sync
        time.sleep(1.2)

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


def get_errors(base_f, resfilename):
    with open(resfilename, 'rb') as resfile:
        results_full=resfile.readlines()

#        [158:238]
    start = 158
    idx=0
    for l in results_full:
        if len(l)<20:
            print "start",idx
            start = idx
            break
        idx+=1

    results = results_full[start:start+80]
    
    errors=0
    for r in results[:-1]:
        if r.find('*')!=-1:
            errors+=1

    if results:
        return (base_f, errors, results[-1])
    else:
        return None

def measure():
    base_f=1206 # works
    base_f=1503

    resfilename='/home/john/basicft8/x'

    res={}

    meas_f = False

    if not meas_f:
        g_hz_per_count=0.9
    else:
        #g_hz_per_count=1.16 # 1200
        g_hz_per_count=1.05

    retry_limit = 5
#    for base_f in range(1190,1210):
    #for base_f in range(1490,1510):
    while g_hz_per_count<1.3:

        retries = 0
        run_ft8(base_f)

        errors = get_errors(base_f, resfilename)

        while not errors and retries < retry_limit:
            print "retrying"
            run_ft8(base_f)
            errors = get_errors(base_f, resfilename)
            retries +=1

        if retries == retry_limit:
            break

        if meas_f:
            res[base_f] = errors
        else:
            res[g_hz_per_count] = errors

        if meas_f:
            print res[base_f]
        else:
            print res[g_hz_per_count]

        if not meas_f:
            g_hz_per_count += 0.01

    init=True
    k=res.keys()
    k.sort()
    for r in k:
        fname="hz1.%s.txt" % repr(base_f)
        if init:
            if os.path.exists(fname):
                os.unlink(fname)
            init=False
        w=open(fname,"a+")
        w.write("%s %s\n" % (repr(r), repr(res[r])))
        print r, res[r]
        w.close()

if __name__ == "__main__":

    base_f=1200

    run_ft8(base_f)

    resfilename='/home/john/basicft8/x'

    print get_errors(base_f, resfilename)
