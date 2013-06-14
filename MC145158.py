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

import sys

class MC145158:
    def __init__(self, prescale_divide, serial_writer, getlock):

        self.m_stream_writer=serial_writer

        self.m_refclk = None

        self.m_r = None

        self.m_prescale_divide = prescale_divide

        self.m_getlock = getlock

        return


    def set_ref_divider(self, divratio):

        divratio_int = int(divratio)

        if divratio_int>4095:
            print "invalid divide ratio",divratio
            sys.exit(1)

        #
        # MC145158-1.pdf page 2
        #
        self.m_stream_writer.output_msb_first((divratio_int<<1) + 1, 15)

        # poke the chip
        self.m_stream_writer.latch()

        self.m_r = divratio_int

        return

    def set_refclk(self, refclk):

        self.m_refclk = refclk

        return

    def set_counter(self, val):
        self.m_counter = val

        #
        # a, n and code (1 or 0) makes 18 bits
        #
        print "writing"
        self.m_stream_writer.output_msb_first((val<<1), 18)

        # poke the chip
        self.m_stream_writer.latch()
        print "written"
        
        return

    def set_freq(self, freq):
        x = (freq * self.m_r)/self.m_refclk

        n = int(x/self.m_prescale_divide)

        a = int(round(x-n*self.m_prescale_divide))

        encoded = (n<<7) + a

        self.set_counter(encoded)

        return

    def enable_phase_comparator(self,lock_detect_enable):
        # # phase comparator
        # d = [0,not lock_detect_enable]

        # c = [0,0]

        # self.m_stream_writer.output_bitlist_lsb_first(d+c)

        # self.m_stream_writer.latch()

        return

    def locked(self):
        #
        # lock detect
        #
        # ld = self.m_port.D5

        return self.m_getlock()



 
 
