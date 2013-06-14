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

class ShiftReg:
    def __init__(self, serial_writer, nbits):

        self.m_serial_writer = serial_writer

        self.m_bits = 0

        self.m_nbits = nbits

        return

    def clearbit(self, bit):
        self.m_bits &= ~bit

        return

    def setbit(self, bit):
        self.m_bits |= bit

        return

    def latch(self):

        self.m_serial_writer.output_msb_first(self.m_bits, self.m_nbits)

        self.m_serial_writer.latch()

        return
        
