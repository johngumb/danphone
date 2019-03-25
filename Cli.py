#
# OpenPMR - tools to make old PMR radios useful.
#
# Copyright (C) 2013  John Gumb, G4RDC
#
# This file is part of OpenPMR.

# OpenPMR is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# OpenPMR is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with OpenPMR.  If not, see <http://www.gnu.org/licenses/>.

import telnetlib
import serial
import time
import sys

import radiosim

class SerialCLI:
    def __init__(self):
        return

class TelnetCLI:
    def __init__(self, mcmicro, server_transport_addr):

#        self.m_tn = telnetlib.Telnet("skate",2217)

        self.m_default_timeout = 0.5

        if '-s' in sys.argv:
            self.m_serial = serial.Serial("/dev/ttyUSB0", 115200, timeout=self.m_default_timeout)
        else:
            self.m_serial = serial.serial_for_url("rfc2217://%s:%d" % server_transport_addr, 115200, timeout=self.m_default_timeout)

        #
        # needed to detect whether the radio has volts attatched
        # - drive DTR to -9V. Radio will pull this up if there is power
        #
        self.m_serial.setDTR(False)

        self.m_mcmicro = mcmicro

        self.m_cmdlist = []

        self.m_cmd_in_progress = False

        # debug
        self.m_current_msg = None

        self.m_last_msg = None

        (self.m_server, _) = server_transport_addr

        return

    def server(self):
        return self.m_server

    def update_power_present(self):
        power_present = self.m_serial.getDSR()

        self.m_mcmicro.set_power_supply_state(power_present)

        return power_present

    def enqueue(self, msg):

        #
        # optimisation
        #
        if msg[0] not in ["Z","D","M","H"]:
            if msg==self.m_last_msg:
                return
            else:
                self.m_last_msg=msg

        if self.m_cmd_in_progress:
            self.m_cmdlist.append(msg)
            print "queued",msg,len(self.m_cmdlist),"elements now queued", "waiting on",self.m_current_msg

        else:
            self.m_cmd_in_progress=True
            self.send(msg)

    def expect(self, char):

        """ simplified expect-like interface """

        chbuf = ""

        while True:
            ch = self.m_serial.read()

#            print "got", ch

            if not ch:
                return (-1, None, None)

            chbuf = chbuf + ch

            if ch == char:
                break

        return (0, True, chbuf)

    def resync(self):
        print "resyncing serial interface"

        startchar = 'a'

        synchar=ord(startchar)

        syncount = 0

        while syncount < 5:

            time.sleep(0.05)

            power_present = self.update_power_present()

            if not power_present:
                break

            ts = 'R'+chr(synchar)+"\n"

            #print ts

            self.m_serial.write(ts)
            
            (idx, mo, txt) = self.expect("Y")

            if idx == -1 and mo == None:
                print "."
                continue

            rxs = txt[-2]

            #
            # sending ahead of receiving, swallow Ys
            # till we sync
            #
            #print rxs,chr(synchar)

            if ord(rxs)<synchar:
                self.expect('Y')
                syncount = 0
            else:
                if rxs == chr(synchar):
                    syncount = syncount + 1
                else:
                    if chr(synchar) == 'z':
                        synchar=ord(startchar)

                    synchar = synchar + 1
                    syncount = 0



    def xmit(self, msg):

        timeout = False

        while True:
            if timeout:
                print "retrying",msg
                time.sleep(0.1)

            if msg=='E01':
                self.m_serial.timeout = 2.0;

            # control commands knocks off msg syncing
            if msg[0]=='C':
                self.m_serial.timeout = self.m_default_timeout
    
            self.m_serial.write(msg+"\n")

            if msg.find("pin1")==0:
                (idx, mo, txt) = (0, None, '')
                break

            #a=time.time()
            (idx, mo, txt) = self.expect("K")
            #print time.time()-a

            #
            # timeout?
            #
            if idx == -1 and mo == None:
                print "xmit timeout on", msg
                timeout=True

                self.resync()
            else:
                break            
            
        return ( idx, mo, txt )

    def send(self, msg):

        #print "sending",msg

        self.m_cmd_in_progress=True

        #print self.m_serial.getCTS()

        # debug
        self.m_current_msg = msg

        (idx, mo, text)=self.xmit(msg)

        if (len(self.m_cmdlist)):
            print "pending cmds, sent",msg

        t = text.strip()

        #print t

        if t:
            # force power to be on for 'M' dac message
            if msg[0]=="M":
                v=int(t[-2],16)|4
                updatestr="%X" % v
            else:
                updatestr=t[:-1]
            self.m_mcmicro.stcharupdate(updatestr)

        while (len(self.m_cmdlist)):
            cmd = self.m_cmdlist.pop(0)
            l=len(self.m_cmdlist)
            print "dequeued",cmd, l, "elements left, sending",cmd
            self.send(cmd)

        # system should notify in case of state change
        #self.xmit("Y")

        self.m_cmd_in_progress=False

    def cmd_in_progress(self):
        return self.m_cmd_in_progress
