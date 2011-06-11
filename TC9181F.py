import sys

import SerialStreamWriter

class TC9181F:
    def __init__(self,port,DATA=0,CLK=0,STB=0,LOCK=0):

        self.m_port = port

        self.m_stream_writer=SerialStreamWriter.SerialStreamWriter(port,DATA,CLK,STB)

        self.m_port.bb.direction &= ~LOCK

        self.m_refclk = None

        self.m_r = None

        return


    def set_ref_divider_bitlist(self,rbitlist):
        #
        # TC9181.PDF section 2
        # L1=0 (no fixed divide by 1024, use serial data)
        # L2=0 (external low pass filter is inverting)
        #
        # TODO figure out what L2 is - depends whether LPF is inverting or non
        # inverting
        #
        l=[0,0]

        # TC9181.PDF section 2 page 5
        c=[0,1]

        # set up data on chip
        self.m_stream_writer.output_bitlist_lsb_first(rbitlist+l+c)

        # poke the chip
        self.m_stream_writer.latch()

        return

    def set_ref_divider(self, divratio):

        if divratio>4095:
            print "invalid divide ratio",divratio
            sys.exit(1)

        #
        # code bit, L1 and L2 are 0
        # 8<<12 = 32768
        #
        self.m_stream_writer.output(divratio | 32768, 16)

        # poke the chip
        self.m_stream_writer.latch()

        self.m_r = divratio

        return

    def set_refclk(self, refclk):

        self.m_refclk = refclk

        return

    def set_counter_bitlist(self,alist,nlist):
        #
        # TC9181.PDF section 3 page 6
        # code bit is 1,0
        #
        c=[1,0]

        # set up data on chip
        self.m_stream_writer.output_bitlist_lsb_first(alist+nlist+c)

        # poke the chip
        self.m_stream_writer.latch()

        return

    def set_counter(self, val):
        self.m_counter = val

        #
        # a, n and code makes 20 bits
        # 1<<18 = 262144
        #
        print "writing"
        self.m_stream_writer.output(val | 262144, 20)

        # poke the chip
        self.m_stream_writer.latch()
        print "written"
        
        return

    def set_freq(self, freq):
        x = (freq * self.m_r)/self.m_refclk

        n = int(x/32)

        a = int(round(x-n*32))

        encoded = (n<<7) + a

        self.set_counter(encoded)

        return

    def enable_phase_comparator(self):
        # phase comparator
        d = [0,0]

        c = [0,0]

        self.m_stream_writer.output_bitlist_lsb_first(d+c)

        self.m_stream_writer.latch()

        return

    def enable_outputs(self,val):

#        d=[1,0,1,1]

        d = val + [1, 1]

        self.m_stream_writer.output_bitlist_lsb_first(d)

        self.m_stream_writer.latch()

        return

 
