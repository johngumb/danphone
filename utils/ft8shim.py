import sys
import os
import socket
import socketserver
import csv

# TODO calibrate dac freq based on beat freq received from FCD???

class FT8symTranslator:
    def __init__(self, basefreq):
        self.m_tones = {}
        for i in range(8):
            self.m_tones[i]=i*6.25 + basefreq

    def sym_to_freq(self, sym):
        return self.m_tones[sym];

class WsjtxListener(socketserver.BaseRequestHandler):
    def get_radio_encoder(self, basefreq):
        self.clear_radio_encoder()

        self.server.m_radio_cmd_encoder = RadioCmdEncoder()
        self.server.m_radio_cmd_encoder.prepare_for_symseq(basefreq)

    def clear_radio_encoder(self):
        if self.server.m_radio_cmd_encoder:
            del self.server.m_radio_cmd_encoder
            self.server.m_radio_cmd_encoder = None

    def handle(self):
        self.data = self.request.recv(1024).strip()

        #print(str(self.data,'ascii'))
        req = str(self.data,'ascii')
        print(req)

        # base freq; prepare
        if req.find("FB")==0:
            print("got fb")

            basefreq = int(req[2:])
            self.get_radio_encoder(basefreq)

            self.server.m_radio_cmd_encoder.send_msg("ft8-txon")

        # message to send; must be prepared otherwise ignore
        elif req[0]=='M':
            if self.server.m_radio_cmd_encoder:
                symlist = [int(a) for a in list(req[1:])]
                self.server.m_radio_cmd_encoder.send_symseq(symlist)
                self.server.m_radio_cmd_encoder.send_msg("ft8-txoff")
            else:
                print("ignoring",req,"not prepared")

            self.clear_radio_encoder()

        elif req.find("TX")==0:
            if self.server.m_radio_cmd_encoder:
                if req[2]=='1':
                    if self.server.m_radio_cmd_encoder.use_pa():
                        self.server.m_radio_cmd_encoder.send_msg("pa-on")
                    else:
                        print(req,"request denied, pa not enabled")
                else:
                    self.server.m_radio_cmd_encoder.request_cancel_tx()
            else:
                print("ignoring",req,"no encoder")
            
        else:
            print("unknown message",req)

class RefOsc:
    def __init__(self, datafile, calfile):

        with open(datafile) as csvfile:
            self.m_reader = csv.reader(csvfile)
            self.m_refosc_lookup = [ row for row in self.m_reader ]

        if os.path.exists(calfile):
            with open(calfile) as caldata:
                self.m_caldata = int(caldata.read())
        else:
            self.m_caldata = 0

        return

    def set_base_freq(self, freq):
        self.m_base_freq = freq
        for row in self.m_refosc_lookup:
            [dacv, dacf] = row

            if freq<float(dacf)-2000:
                self.m_base_dac = int(dacv)+self.m_caldata
                break

        self.m_last_freq = None

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

        twice_dac_val = twice_dac + twice_dac_offset
        if twice_dac_val % 2 == 0:
            dac_val = int(twice_dac_val/2)
            dac_cmd = 2
        else:
            # divisible by 2
            twice_dac_val_minus_1_over_2 = int((twice_dac_val-1)/2)

            # will overflow when 1 added at remote end?
            if (twice_dac_val_minus_1_over_2 & 0xFF) == 0xFF:
                dac_val = twice_dac_val_minus_1_over_2+2
                # subtract one at remote
                dac_cmd = 3
            else:
                dac_val = twice_dac_val_minus_1_over_2
                # add one at remote
                dac_cmd = 4

        result = (dac_cmd, dac_val)

        #print result

        self.m_last_freq = freq
        self.m_last_sym = sym

        return result

class RadioCmdHandler:
    def __init__(self):
        self.m_response_socket_name = "/tmp/ft8response"
        self.setup_response_socket(self.m_response_socket_name)

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
        s.connect("/tmp/mui-ext.s.6m")
        s.sendall(asciimsg)
        s.close()

        conn, addr = self.m_response_server.accept()
        datagram = conn.recv(1024)
        assert(datagram==asciimsg)
        #print "got response",datagram

class RadioCmdEncoder:
    def __init__(self):
        self.m_radio_cmd_handler=RadioCmdHandler()
        self.m_use_pa = sys.argv[1] == "-p"
        self.m_sync_dac_cmds = False
        self.m_cancel_tx = False

    def prepare_for_symseq(self, basefreq):

        # think about lifetime
        self.m_refosc = RefOsc("dacdata-orig.csv", "/home/john/6mcal")

        self.m_ft8trans = FT8symTranslator(basefreq)

        self.m_refosc.set_base_freq(basefreq)

    def request_cancel_tx(self):
        self.m_cancel_tx = True

    def use_pa(self):
        return self.m_use_pa

    # allow access to raw message sender
    def send_msg(self, msg):
        self.m_radio_cmd_handler.send_msg(msg)

    def send_dac(self, val):
        '''
        takes a tuple (N,DV) where N is DAC command (1,2,3,4) and DV is 12 bit dac val
        '''
        cmd = "D%d%X" % val
        print(cmd)
        self.m_radio_cmd_handler.send_msg(cmd)

    def cancel_tx(self):
        self.m_radio_cmd_handler.send_msg("ft8-txoff")
        self.m_radio_cmd_handler.send_msg("E0000") # stop 160ms sync
        self.m_cancel_tx = False

    def send_symseq(self, symseq):
        # turn on 160ms sync on dac commands
        self.m_radio_cmd_handler.send_msg("EA19F")

        for sym in symseq:
            if self.m_cancel_tx:
                self.cancel_tx()
                break;

            symfreq = self.m_ft8trans.sym_to_freq(sym)

            d = self.m_refosc.freq_to_dac(sym, symfreq)

            self.send_dac(d)

        self.m_radio_cmd_handler.send_msg("E0000") # stop 160ms sync


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
        server.serve_forever()

    except KeyboardInterrupt:
        print("interrupt")
        server.shutdown()
        server.socket.close()
        os.unlink(wsj_listen_sock)

if __name__ == "__main__":
    wsj_listen_sock = "/tmp/testsock"

    establish_wsjtx_listener(wsj_listen_sock);
