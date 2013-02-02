import time

class SerialStreamWriter:
    def __init__(self,hwif):
        self.m_hwif=hwif

        self.m_debug = False

        return

    def clockpulse(self):

        # generate clock pulse on clock pin
        self.m_hwif.risingedge(self.m_clk)

        return
        
    def outbit(self,valn):

        if valn==0:
            val=False
        else:
            val=True

        if self.m_debug:
            print valn

        # set required data on data pin
        self.m_hwif.setboolbit(self.m_data, val)

        self.clockpulse()

        return

    def output_bitlist_lsb_first(self,bitlist):
        for x in bitlist:
            self.outbit(x)

    def output(self,val,nbits):
        finmask = 1 << nbits

        mask = 1

        while mask != finmask:

            self.m_hwif.setboolbit(self.m_data, val & mask)

            if self.m_debug:
                if val & mask:
                    print 1
                else:
                    print 0

            mask = mask << 1

            self.clockpulse()

        return

    def output_msb_first(self, val, nbits):
        mask = 1 << (nbits - 1)

        while mask != 0:

            self.m_hwif.setboolbit(self.m_data, val & mask)

            if self.m_debug:
                if val & mask:
                    print 1
                else:
                    print 0

            mask = mask >> 1

            self.clockpulse()

        return

class SerialStreamWriterFTDI(SerialStreamWriter):
    def __init__(self,hwif,DATA=0,CLK=0,STB=0):
        SerialStreamWriter.__init__(self, hwif)

        self.m_data = DATA
        self.m_clk = CLK
        self.m_stb = STB

        dirval=(DATA|CLK|STB)

        # FIXME
        if self.m_hwif:
            self.m_hwif.bb.direction |= dirval

        return

    def latch(self):

        # generate strobe pulse
        self.m_hwif.pulsebithigh(self.m_stb)

        return
  

class SerialStreamWriterCLI(SerialStreamWriter):
    def __init__(self, hwif, ident):
        SerialStreamWriter.__init__(self, hwif)

        self.m_ident = ident

        return

    def output_msb_first(self, val, nbits):


        hexval="%X" % val

        nibbles_val=len(hexval)
        if (nbits % 4) == 0:
            nibbles_required = nbits/4
        else:
            nibbles_required = (nbits/4) + 1

        zeropad = nibbles_required - nibbles_val

        self.m_hwif.enqueue(self.m_ident + '0'*zeropad + hexval)

        return

    def latch(self):

        # just a dummy routine for CLI based writers
        # as the 
        # press CR?
        #self.m_hwif.enter()

        return
