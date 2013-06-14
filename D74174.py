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

import ft232r

class D74174:
    def __init__(self, hwif, latchbit, mask):
        assert(hwif)

        self.m_hwif = hwif

        self.m_latchbit = latchbit

        self.m_inverse_mask = ~mask

        self.m_bits = 0

        return

    def clearbit(self, bit):
        self.m_bits &= ~bit

        return

    def setbit(self, bit):
        self.m_bits |= bit

        return

    def latch(self,downtime=0):

        curval = self.m_hwif.getport()

        # knock out mask
        curval &= self.m_inverse_mask

        # put in our stuff
        curval |= self.m_bits

        # put data on pins
        self.m_hwif.setport(curval)

        # 
        # latch data onto output pins of 74174
        #
        self.m_hwif.bb.direction |= self.m_latchbit
        self.m_hwif.pulsebitlow(self.m_latchbit,downtime)

        
