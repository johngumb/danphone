import ft232r
import TC9181F
import D74174
import time


class DanPhone:

    def __init__(self):
        self.m_hwif=ft232r.ft232r()

        #
        # peripheral interface
        #
        self.m_74174 = D74174.D74174(self.m_hwif, self.m_hwif.D7, \
                                         self.m_hwif.D0|self.m_hwif.D1)

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

        self.m_rxsynth.enable_phase_comparator(True)

        self.m_tx_freq = None

        # initial states
        self.m_rx_attenuate = False

        self.disable_tx()

        self.disable_tx_drive()

        self.disable_pa()

        self.m_step = None

        return

    def set_step(self,step):

        self.m_step = step

        self.m_divratio = int(round(self.m_refclk / step))

        self.set_ref_divider( self.m_divratio )
        
        return

    def init_tx(self):

        self.m_txsynth.set_refclk(self.m_refclk)

        self.m_txsynth.enable_phase_comparator(True)

        self.m_txsynth.set_ref_divider(self.m_divratio)

        if self.m_tx_freq:
            self.m_txsynth.set_freq(self.m_tx_freq)
            
        return

    def init_rx(self):

        self.m_rxsynth.set_refclk(self.m_refclk)

        self.m_rxsynth.enable_phase_comparator(True)

        self.m_rxsynth.set_ref_divider(self.m_divratio)

        return

    def set_ref_divider(self,divratio):

        self.m_divratio = divratio

        return

    def disable_tx(self):

        self.m_tx_enabled = False

        #
        # ask rx TC9181F to set GPIO pins
        #
        self.m_rxsynth.enable_outputs([self.m_tx_enabled,not self.m_rx_attenuate])

        self.m_74174.latch()

        return
        
    def enable_tx(self):

        self.m_tx_enabled = True

        #
        # ask rx TC9181F to set GPIO pins
        #
        self.m_rxsynth.enable_outputs([self.m_tx_enabled,not self.m_rx_attenuate])

        self.m_74174.latch()

        self.init_tx()

        return

    def disable_tx_drive(self):

        self.m_tx_drive_enabled = False

        self.m_74174.clearbit(self.m_hwif.D1)

        self.m_74174.latch()

        # rh pin (pin 3) is ele 0
        # lh pin (pin 14) is ele 1
        self.m_txsynth.enable_outputs([0,0])
        self.m_74174.latch()

        # leave D1 with junk for now
        return
        
    def enable_tx_drive(self):

        self.m_tx_drive_enabled = True

        self.m_74174.setbit(self.m_hwif.D1)

        self.m_txsynth.enable_outputs([1,1])

        self.m_74174.latch()

        # leave D1 with junk for now
        return

    def disable_pa(self):

        self.m_pa_enabled = False

        self.m_74174.clearbit(self.m_hwif.D0)

        self.m_74174.latch()

        # leave D0 with junk for now
        return
        
    def enable_pa(self):

        # TODO make a proper lockout list
        if not self.m_tx_freq in [70387500.0,70412500.0]:
            self.m_pa_enabled = True

            self.m_74174.setbit(self.m_hwif.D0)

            self.m_74174.latch()

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

        # TODO merge rx/tx locked functions to cut down on usb traffic
        # i.e. the purge
        # consider local purge function which executes a purge based
        # on when last one was called
        #
        self.m_hwif.bb.ftdi_fn.ftdi_usb_purge_rx_buffer()

        if not self.m_tx_enabled:
            result = False
        else:
            result = ((self.m_hwif.bb.port & self.m_hwif.D5) == 0)

        return result


    def rx_locked(self):

        # TODO merge rx/tx locked functions to cut down on usb traffic
        # i.e. the purge
        self.m_hwif.bb.ftdi_fn.ftdi_usb_purge_rx_buffer()

        result = ((self.m_hwif.bb.port & self.m_hwif.D4) == 0)

        return result

    def powered_on(self):
        #
        # HACK
        # the following (i.e. setting D7 as an input)
        # upsets the D74174 latch a bit
        # so make sure any data which may get latched through is correct.
        # Just calling the 74174 latch function does this.
        #
        self.m_74174.latch()

        # D7 as output (probably)
        tmpdir = self.m_hwif.bb.direction

        # D7 now an input
        self.m_hwif.bb.direction &= ~self.m_hwif.D7

        # HACK throw away result
        self.m_hwif.bb.ftdi_fn.ftdi_usb_purge_rx_buffer()

        # HACK throw away result
        self.m_hwif.bb.port

        # HACK following result is good
        self.m_hwif.bb.ftdi_fn.ftdi_usb_purge_rx_buffer()

        # HACK this result is good
        result = ((self.m_hwif.bb.port & self.m_hwif.D7) == self.m_hwif.D7)

        # back to D7 as output
        self.m_hwif.bb.direction = tmpdir

        # BEWARE anything read as input may be dodgy until
        # ftdi_usb_purge_rx_buffer() is called

        return result

    def get_hwif(self):
        return self.m_hwif

    def squelch_open(self):
        self.m_hwif.bb.ftdi_fn.ftdi_usb_purge_rx_buffer()

        result = ((self.m_hwif.bb.port & self.m_hwif.D6) == self.m_hwif.D6)

        return result

    def disable_rx(self):

        self.m_rx_attenuate = True

        #
        # ask rx TC9181F to set GPIO pins
        #
        self.m_rxsynth.enable_outputs([self.m_tx_enabled,not self.m_rx_attenuate])

        self.m_74174.latch()

        self.init_tx()

        return

    def enable_rx(self):
        self.m_rx_attenuate = False

        #
        # ask rx TC9181F to set GPIO pins
        #
        self.m_rxsynth.enable_outputs([self.m_tx_enabled,not self.m_rx_attenuate])

        self.m_74174.latch()

        self.init_tx()

        return
