import ft232r
import TC9181F
import time

class DanPhone:

    def __init__(self):
        self.m_hwif=ft232r.ft232r()

        #
        # clock to 74174 latch must be kept high
        #
        self.m_hwif.initialise(direction=self.m_hwif.D7, value=self.m_hwif.D7)

        self.m_txsynth = TC9181F.TC9181F(self.m_hwif,\
                                  DATA=self.m_hwif.D0,\
                                  CLK=self.m_hwif.D1,\
                                  STB=self.m_hwif.D3,\
                                  LOCK=self.m_hwif.D5)

        self.m_rxsynth = TC9181F.TC9181F(self.m_hwif,\
                                  DATA=self.m_hwif.D0,\
                                  CLK=self.m_hwif.D1,\
                                  STB=self.m_hwif.D2,\
                                  LOCK=self.m_hwif.D4)
        self.m_refclk = 12.8E6

        self.m_rxsynth.set_refclk(self.m_refclk)

        self.m_rxsynth.enable_phase_comparator()

#        self.m_rxsynth.enable_outputs()

        self.m_tx_freq = None

        self.m_tx_enabled = False

        self.m_step = None

        return

    def set_step(self,step):

        self.m_step = step

        self.m_divratio = int(round(self.m_refclk / step))

        self.set_ref_divider( self.m_divratio )
        
        return

    def init_tx(self):

        self.m_txsynth.set_refclk(self.m_refclk)

        self.m_txsynth.enable_phase_comparator()

        self.m_txsynth.set_ref_divider(self.m_divratio)

#        self.m_txsynth.enable_outputs()

        if self.m_tx_freq:
            self.m_txsynth.set_freq(self.m_tx_freq)
            
        return

    def init_rx(self):

        self.m_rxsynth.set_refclk(self.m_refclk)

        self.m_rxsynth.enable_phase_comparator()

        self.m_rxsynth.set_ref_divider(self.m_divratio)

        return

    def set_ref_divider(self,divratio):

        self.m_divratio = divratio

        return

    def disable_tx(self):

        self.m_tx_enabled = False

        #
        # ask rx TC9181F to set GPIO pins low
        #
        self.m_rxsynth.enable_outputs([0,0])

        # 
        # latch data onto output pins of 74174
        #
        self.m_hwif.pulsebitlow(self.m_hwif.D7)

        return
        
    def enable_tx(self):

        self.m_tx_enabled = True

        #
        # ask rx TC9181F to set GPIO pins high
        #
        self.m_rxsynth.enable_outputs([1,1])

        # 
        # latch data onto output pins of 74174
        #
        self.m_hwif.pulsebitlow(self.m_hwif.D7)

        self.init_tx()

        return

    def disable_tx_drive(self):

        self.m_tx_drive_enabled = False

        curval = self.m_hwif.getport()

        newval = curval & ~self.m_hwif.D1

        self.m_hwif.setport(newval)

        # latch
        self.m_hwif.pulsebitlow(self.m_hwif.D7)

        # leave D0 with junk for now
        return
        
    def enable_tx_drive(self):

        self.m_tx_drive_enabled = True

        curval = self.m_hwif.getport()

        newval = curval | self.m_hwif.D1

        self.m_hwif.setport(newval)

        # latch
        self.m_hwif.pulsebitlow(self.m_hwif.D7)

        # leave D0 with junk for now
        return

    def tx_enabled(self):

        return self.m_tx_enabled

    def set_rx_freq(self,freq):

        self.m_rxsynth.set_freq(freq+21.4E6)
                
        return

    def set_tx_freq(self,freq):

        self.m_tx_freq = freq

        if self.m_tx_enabled:
            self.m_txsynth.set_freq(freq)

        return

    def tx_locked(self):

        self.m_hwif.bb.ftdi_fn.ftdi_usb_purge_rx_buffer()

        if not self.m_tx_enabled:
            result = False
        else:
            result = ((self.m_hwif.bb.port & self.m_hwif.D5) == 0)

        return result


    def rx_locked(self):

        self.m_hwif.bb.ftdi_fn.ftdi_usb_purge_rx_buffer()

        result = ((self.m_hwif.bb.port & self.m_hwif.D4) == 0)

        return result

    def get_hwif(self):
        return self.m_hwif

    def squelch_open(self):
        self.m_hwif.bb.ftdi_fn.ftdi_usb_purge_rx_buffer()

        result = ((self.m_hwif.bb.port & self.m_hwif.D6) == self.m_hwif.D6)

        return result
