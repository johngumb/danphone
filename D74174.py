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

    def latch(self):

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
        self.m_hwif.pulsebitlow(self.m_hwif.D7)

        
