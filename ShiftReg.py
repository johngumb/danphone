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

        self.m_serial_writer.output(self.m_bits, self.m_nbits)

        self.m_serial_writer.latch()

        return
        
