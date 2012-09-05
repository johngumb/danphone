import sys

import SerialStreamWriter

class MC145158:
    def __init__(self, port, prescale_divide, DATA=0,CLK=0,STB=0,LOCK=0):

        self.m_port = port

        self.m_stream_writer=SerialStreamWriter.SerialStreamWriter(port,DATA,CLK,STB)

        # FIXME
        self.m_port.bb.direction &= ~LOCK

        self.m_refclk = None

        self.m_r = None

        self.m_prescale_divide = prescale_divide

        self.m_lockbit = LOCK

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

        self.m_port.bb.ftdi_fn.ftdi_usb_purge_rx_buffer()

        result = ((self.m_port.bb.port & self.m_lockbit) == self.m_lockbit)

        return result

 
 
