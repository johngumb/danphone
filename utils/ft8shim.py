import os
import socketserver
import csv

class WsjtxListener(socketserver.BaseRequestHandler):
    def handle(self):
        self.data = self.request.recv(1024).strip()

        #print(str(self.data,'ascii'))
        req = str(self.data,'ascii')
        if req.find("FB")==0:
            self.server.m_refosc.set_base_freq(int(req[2:]))
        #print(self.server.arg1)
        print(self.server.arg2)
        return

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
            dac_val = twice_dac_val/2
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

class RadioCmdSender():
    def __init__(self):
        return


class RadioUnixStreamServer(socketserver.UnixStreamServer):

    def __init__(self, server_address, RequestHandlerClass, refosc, arg2):
        socketserver.UnixStreamServer.__init__(self, 
                                                 server_address, 
                                                 RequestHandlerClass)
        self.m_refosc = refosc
        self.arg2 = arg2

def establish_wsjtx_listener(sockname, refosc):
    if os.path.exists(wsj_listen_sock):
        os.unlink(wsj_listen_sock)

#    server = socketserver.UnixStreamServer(wsj_listen_sock, WsjtxListener)
    server = RadioUnixStreamServer(wsj_listen_sock, WsjtxListener, refosc, 2)

    try:
        server.serve_forever()

    except KeyboardInterrupt:
        print("interrupt")
        server.shutdown()
        server.socket.close()
        os.unlink(wsj_listen_sock)

if __name__ == "__main__":
    wsj_listen_sock = "/tmp/testsock"


    refosc = RefOsc("dacdata-orig.csv", "/home/john/6mcal")

    establish_wsjtx_listener(wsj_listen_sock, refosc);
