import sys
import os
import socket
import socketserver
import csv
import subprocess
import time
from datetime import datetime
import math

# TODO calibrate dac freq based on beat freq received from FCD???

def cal_value(band):
    val = 0
    if band == "70cm":
        actband = "2m"
    else:
        actband = band

    calfile="/home/john/%scal" % actband
    if os.path.exists(calfile):
        with open(calfile) as caldata:
            val = int(caldata.read())
    return val

class FT8symTranslator:
    def __init__(self, basefreq):
        self.m_tones = {}
        for i in range(8):
            self.m_tones[i]=i*6.25 + basefreq

    def sym_to_freq(self, sym):
        return self.m_tones[sym];

class WsjtxListener(socketserver.BaseRequestHandler):

    def get_radio_encoder(self, basefreq, band):
        self.clear_radio_encoder()

        if band in ["6m","4m", "2m", "70cm"]:
            self.server.m_radio_cmd_encoder = RadioCmdEncoder()
            self.server.m_radio_cmd_encoder.prepare_for_symseq(basefreq, band)

    def clear_radio_encoder(self):
        if self.server.m_radio_cmd_encoder:
            del self.server.m_radio_cmd_encoder
            self.server.m_radio_cmd_encoder = None

    def handle(self):
        self.data = self.request.recv(1024).strip()

        # received something, cancel timeout timer
        self.server.socket.settimeout(None)
        self.server.m_timeout_set = False
        self.server.m_set_fb = False

        #print(str(self.data,'ascii'))
        req = str(self.data,'ascii')
        print(req)

        if not req:
            print("Empty request string received")
            return

        # base freq; prepare, frequency base
        if req.find("FB")==0:
            print("got prepare",time.asctime())

            basefreq_and_band = req[2:].split(',')
            basefreq = int(basefreq_and_band[0])
            band = basefreq_and_band[1].strip()

            self.get_radio_encoder(basefreq, band)

            if not self.server.m_radio_cmd_encoder:
                print("Band", band, "not supported")

            self.server.m_set_fb = True
            self.server.m_timeout_set = True
            self.server.socket.settimeout(3.5)

        # message to send; must be prepared otherwise ignore
        elif req[0]=='M':
            print(time.asctime())
            print(datetime.utcnow())
            if self.server.m_radio_cmd_encoder:
                symlist = [int(a) for a in list(req[1:])]
                self.server.m_radio_cmd_encoder.send_symseq(symlist)
                self.server.m_radio_cmd_encoder.cancel_tx()
            else:
                print("ignoring",req,"not prepared")

            self.clear_radio_encoder()

        # PTT control  message
        elif req.find("TX")==0:
            if self.server.m_radio_cmd_encoder:
                if req[2]=='1':
                    if self.server.m_radio_cmd_encoder.use_pa():
                        self.server.m_radio_cmd_encoder.send_msg("pa-on")
                    else:
                        print(req,"request denied, pa not enabled")
                else:
                    print(time.asctime())
                    self.server.m_radio_cmd_encoder.cancel_tx()
            else:
                print("ignoring",req,"no encoder")
            
        else:
            print("unknown message",req)

class RefOsc6m:
    def __init__(self, datafile, calfile):

        with open(datafile) as csvfile:
            self.m_reader = csv.reader(csvfile)
            self.m_refosc_lookup = [ row for row in self.m_reader ]

        if os.path.exists(calfile):
            with open(calfile) as caldata:
                self.m_caldata = int(caldata.read())
        else:
            self.m_caldata = 0

        self.m_last_freq = None
        self.m_last_sym = None

        return

    def set_base_freq(self, freq):
        self.m_base_freq = freq
        for row in self.m_refosc_lookup:
            [dacv, dacf] = row

            if freq<float(dacf)-2000:
                self.m_base_dac = int(dacv)+self.m_caldata
                break

        self.m_last_freq = None
        self.m_last_sym = None

        return

    # maybe get rid of sym eventually?
    def freq_to_dac(self, sym, freq):
        if self.m_base_freq >= 2300:
            hz_per_count = 1.03
        else:
            hz_per_count = 1.05

        dac=self.m_base_dac + (freq-self.m_base_freq)*hz_per_count

        twice_dac = 2 * dac

        if self.m_last_freq:
            diff_offset = freq - self.m_last_freq
        else:
            diff_offset = 0

        #17 nov twice_dac_offset=(2*(freq_offset+(diff_offset/4.2)+square_offset))/hz_per_count

        #4.17 looks good 17 nov
        if diff_offset>0:
            factor=4.17
        else:
            factor=4.17

        twice_dac_offset=(2*diff_offset/factor)/hz_per_count

        if sym > 4 and self.m_last_sym >= 4:
            twice_dac_offset += 2

        twice_dac = int(round(twice_dac))

        twice_dac_val = twice_dac + twice_dac_offset + (cal_value("6m")*2)

        if twice_dac_val % 2 == 0:
            dac_val = int(twice_dac_val/2)
            dac_cmd = "2"
        else:
            # divisible by 2
            twice_dac_val_minus_1_over_2 = int((twice_dac_val-1)/2)

            # will overflow when 1 added at remote end?
            if (twice_dac_val_minus_1_over_2 & 0xFF) == 0xFF:
                dac_val = twice_dac_val_minus_1_over_2+2
                # subtract one at remote
                dac_cmd = "3"
            else:
                dac_val = twice_dac_val_minus_1_over_2
                # add one at remote
                dac_cmd = "4"

        result = ("D", dac_cmd, dac_val)

        self.m_last_freq = freq
        self.m_last_sym = sym

        return result

# uses max5216 DAC
class RefOsc2m:
    def __init__(self, band, calfile):

        if os.path.exists(calfile):
            with open(calfile) as caldata:
                self.m_caldata = int(caldata.read())
        else:
            self.m_caldata = 0

        self.m_band = band
        self.m_last_freq = None
        self.m_last_sym = None
        self.m_last_dac = None

        if band == "2m":
            self.m_fudge_factor = 0.7
            self.m_params = [1.73155253e-01, -1.76378121e+05, 3.30539038e+04, -6.43300940e+03]
        if band == "4m":
            # look for drop to -16
            # 0.3 + (2.2-0.3)/2
            self.m_fudge_factor = 1.25
            self.m_params = [8.94132636e-02, -1.34659984e+07, 3.74200375e+03, -2.08969879e+03]

    def set_base_freq(self, freq):
        return

    def f_to_dv(self, F):
        #
        # from dacdata dacdata-2m-step1-opamp-linear.csv by curve fitting
        # using (a * x) + (b/(x-c)) + d
        #
        # [a, b, c, d] = [1.74150917e-01,
        #                 -4.43158994e+03,
        #                 3.69915130e+04,
        #                 -6.49876296e+03]

    #    [a, b, c, d] = [1.74150917e-01, -4.43158994e+03, 3.69915130e+04, -6.49876296e+03]
        [a,b,c,d] = self.m_params

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

    # maybe get rid of sym eventually?
    def freq_to_dac(self, sym, freq):

        dac = int(self.f_to_dv(freq)) + self.m_caldata
#        dac = math.ceil(f_to_dv(freq)) + self.m_caldata

        if not self.m_last_freq:
            self.m_last_freq = freq
            self.m_last_sym = sym
            self.m_last_dac = dac
            return ("M","", dac)

        #
        # Equating a dac offset directly to a frequency
        # but a direct conversion i.e. factor of 1.0
        # seems fine.
        #
        if self.m_last_freq:
            dac_offset = (freq - self.m_last_freq) * self.m_fudge_factor
        else:
            dac_offset = 0

        #print(dac_offset)

        dac_val = dac + dac_offset

#        if self.m_last_sym == sym:
#            dac_val = self.m_last_dac

        result = ("M", "", int(dac_val))

        self.m_last_freq = freq
        self.m_last_sym = sym
        self.m_last_dac = dac_val

        return result


class RadioCmdHandler:
    def __init__(self, band):
        self.m_response_socket_name = "/tmp/ft8response"
        self.setup_response_socket(self.m_response_socket_name)
        self.m_band = band

    def __del__(self):
        if os.path.exists(self.m_response_socket_name):
            os.remove(self.m_response_socket_name)

    def setup_response_socket(self, socket_name):

        sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        self.m_response_server = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)

        if os.path.exists(socket_name):
            os.remove(socket_name)

        self.m_response_server.bind(socket_name)

    def send_msg(self, msg):
        self.m_response_server.listen(1)
        asciimsg = msg.encode('ascii')
        s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        if self.m_band == "70cm":
            radband = "2m"
        else:
            radband = self.m_band
        s.connect("/tmp/mui-ext.s.%s" % radband)
        s.sendall(asciimsg)
        s.close()

        conn, addr = self.m_response_server.accept()
        datagram = conn.recv(1024)
        assert(datagram==asciimsg)
        #print "got response",datagram

class RadioCmdEncoder:
    def __init__(self):
        self.m_use_pa = "-p" in sys.argv
        self.m_monitor = "-m" in sys.argv
        self.m_sync_dac_cmds = False
        self.m_cancel_tx = False
        self.m_recfile = '/home/john/ft8_t2.wav'
        self.m_recfile_final = '/home/john/ft8_t2_sox.wav'
        self.m_tx_on = False

    def __del__(self):
        if self.m_tx_on:
            self.send_msg("ft8-txoff")
            self.m_tx_on = False

    def request_cancel_tx(self):
        self.m_cancel_tx = True

    def use_pa(self):
        return self.m_use_pa

    # allow access to raw message sender
    def send_msg(self, msg):
        self.m_radio_cmd_handler.send_msg(msg)

    def send_dac(self, val):
        '''
        takes a tuple (C,N,DV) where C is DAC command (either "D" or "M"),
        N is DAC subcommand ("1","2","3","4","") and DV is 12 or 16 bit dac val
        '''
        cmd = "%s%s%X" % val
        print(cmd)
        self.m_radio_cmd_handler.send_msg(cmd)

    def cancel_tx(self):
        # stop 160ms sync
        self.m_radio_cmd_handler.send_msg("E0000")

        if not self.m_tx_on:
            print("cancel tx while tx not active")

        self.m_radio_cmd_handler.send_msg("ft8-txoff")
        self.m_tx_on = False

        # HACK HACK FIXME zero rx should be done in one place
        # where we are for receive on DAC
        if self.m_band == "6m":
            zero_rx = 0xC3E + cal_value(self.m_band) # for rx
            self.m_radio_cmd_handler.send_msg("D2%X" % zero_rx)
        elif self.m_band == "4m":
            zero_rx = 0xBF80 + cal_value(self.m_band)
            self.m_radio_cmd_handler.send_msg("M%X" % zero_rx)
        elif self.m_band in ["2m","70cm"]:
            zero_rx = 0xBF00 + cal_value(self.m_band)
            self.m_radio_cmd_handler.send_msg("M%X" % zero_rx)

        self.m_cancel_tx = False

    def prepare_for_symseq(self, basefreq, band):

        # think about lifetime
        self.m_radio_cmd_handler=RadioCmdHandler(band)

        if band == "70cm":
            calfile = "/home/john/2mcal"
        else:
            calfile = "/home/john/%scal" % band

        # duck typing
        if band == "6m":
            self.m_refosc = RefOsc6m("dacdata-orig.csv", calfile)
        elif band in ["4m", "2m", "70cm"]:
            # FIXME absolute paths
            self.m_refosc = RefOsc2m(band, calfile)

        self.m_ft8trans = FT8symTranslator(basefreq)

        self.m_refosc.set_base_freq(basefreq)

        zero_tx_dac = self.m_refosc.freq_to_dac(0, basefreq)

        print(band)

        if band == "6m":
            self.m_sync_cmd = "EA19F"
        else:
            self.m_sync_cmd = "EA320"

        self.m_band = band

        # allow time to settle
        self.send_msg("ft8-txon")
        self.m_tx_on = True

        self.send_dac(zero_tx_dac)


    def send_symseq(self, symseq):

        if self.m_monitor:
            self.m_recproc = subprocess.Popen(['jack_capture', '-as', '--port', 'sdr_rx:ol', self.m_recfile ])

        # turn on 160ms sync on dac commands
        self.m_radio_cmd_handler.send_msg(self.m_sync_cmd)

        #
        # send the symbol sequence
        #
        for sym in symseq:
            if self.m_cancel_tx:
                self.cancel_tx()
                break;

            symfreq = self.m_ft8trans.sym_to_freq(sym)

            d = self.m_refosc.freq_to_dac(sym, symfreq)

            self.send_dac(d)

        #self.m_radio_cmd_handler.send_msg("E0000") # stop 160ms sync

        if self.m_monitor:
            time.sleep(0.9)
            self.cancel_tx()
            self.m_recproc.terminate()
            status = self.m_recproc.communicate()
            print(status)
            retcode = self.m_recproc.wait()
            print(retcode)
            os.system(str.join(' ', ['sox', self.m_recfile, '-t wavpcm --rate 12k -c 1 -b 16', self.m_recfile_final, '&&', 'rm -f', self.m_recfile]))

            os.system(str.join(' ', ['python', '/home/john/basicft8/basicft8.py', '/home/john/ft8_t2_sox.wav', '>', '/home/john/basicft8/x']))


class WSJTXUnixStreamServer(socketserver.UnixStreamServer):

    def __init__(self, server_address, RequestHandlerClass, radio_cmd_encoder, arg2):
        socketserver.UnixStreamServer.__init__(self, 
                                                 server_address, 
                                                 RequestHandlerClass)
        self.m_radio_cmd_encoder = radio_cmd_encoder
        self.arg2 = arg2

def establish_wsjtx_listener(sockname):
    if os.path.exists(wsj_listen_sock):
        os.unlink(wsj_listen_sock)

#    server = socketserver.UnixStreamServer(wsj_listen_sock, WsjtxListener)
    server = WSJTXUnixStreamServer(wsj_listen_sock, WsjtxListener, None, 2)

    try:
        while True:
            server.handle_request()

            if server.m_radio_cmd_encoder and server.m_timeout_set:
                if server.m_set_fb:
                    server.m_set_fb = False
                else:
                    print("Frequency Base timeout")
                    server.m_radio_cmd_encoder.cancel_tx()
                    del server.m_radio_cmd_encoder
                    server.m_radio_cmd_encoder = None

                    server.socket.settimeout(None)
                    server.m_timeout_set = False

    except KeyboardInterrupt:
        print("interrupt")
        server.shutdown()
        server.socket.close()
        os.unlink(wsj_listen_sock)

if __name__ == "__main__":
    wsj_listen_sock = "/tmp/testsock"

    #establish_wsjtx_listener(wsj_listen_sock);
    r2m = RefOsc2m("dacdata.csv","/tmp/nullcalfile")

    establish_wsjtx_listener(wsj_listen_sock);
    #r2m = RefOsc2m("dacdata.csv","/tmp/nullcalfile")

    #bf=748
    #r2m.set_base_freq(bf)
    #v = r2m.freq_to_dac(0,bf)
    #print(v)
