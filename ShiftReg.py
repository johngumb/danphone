import ft232r
import SerialStreamWriter

class ShiftReg:
    def __init__(self, hwif, clock, data, latch, nbits):
        assert(hwif)

        self.m_hwif = hwif

        self.m_serial_writer = SerialStreamWriter.SerialStreamWriter(hwif, DATA=data, CLK=clock, STB=latch)

        self.m_bits = nbits

        self.m_nbits = nbits

        return

    def clearbit(self, bit):
        self.m_bits &= ~bit

        return

    def setbit(self, bit):
        self.m_bits |= bit

        return

    def latch(self,downtime=0):

        self.m_serial_writer.output(self.m_bits, self.m_nbits)

        self.m_serial_writer.latch()

        return
        
