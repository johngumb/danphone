import ft232r
import ShiftReg
import MC145158
import SerialStreamWriter
import Cli
import threading

import time

class StatusMonitor(threading.Thread):

    def __init__(self, group=None, target=None, name=None,
                 args=(), kwargs=None, verbose=None):
        threading.Thread.__init__(self, group=group, target=target, name=name,
                                  verbose=verbose)

        self.m_args = args

    def run(self,args=()):

        mcmicro = self.m_args

        while not mcmicro.m_request_thread_exit:
            time.sleep(0.3)

            #
            # no need to queue a status command
            # if there is already one in progress
            # as status comes back on all commands
            #
            if not mcmicro.m_hwif.cmd_in_progress():
                mcmicro.getstatus()

# could leave tx unlocked to prevent TX PA enable
class McMicro:
    def __init__(self):
        self.SR_AUDIO_PA=0x01
        self.SR_POWER=0x02 # pin 5
        self.SR_TX_RX=0x04 # pin 6, ensure PA stays off initially
        self.SR_TX_POWER_HI_LO=0x08
        self.SR_EXT_ALARM=0x10
        self.SR_TX_PA=0x20
        self.SR_TX_AUDIO_ENABLE=0x40
        self.SR_RX_AUDIO_ENABLE=0x80

        # Tx/Rx brought out on pin 1 of 15 way d type

        #0x80 pin 4
        #0x10 pin 7 hi/lo power ??

        #0x01 pin 11
        #0x02 pin 12
        #0x04 pin 13
        #0x08 pin 14
        
        self.m_synth_refclk = 14.4E6

        self.m_rx_freq = None

        self.m_tx_freq = None

        self.m_last_status = 0

        self.m_request_thread_exit = False

        return

    def __del__(self):
        self.m_request_thread_exit = True

        self.disable_tx()

        self.setpower(False)

        return

    def setpower(self, val):
        if val:
            self.m_shiftreg.setbit(self.SR_POWER)
        else:
            self.m_shiftreg.clearbit(self.SR_POWER)

        self.m_shiftreg.latch()

        self.disable_tx()

        return

    def enable_tx(self):
        self.m_shiftreg.setbit(self.SR_TX_RX|self.SR_TX_AUDIO_ENABLE)

        self.m_shiftreg.latch()

        self.tune(self.m_tx_freq)

        return

    def disable_tx(self):
        self.m_shiftreg.clearbit(self.SR_TX_RX|self.SR_TX_AUDIO_ENABLE)

        self.m_shiftreg.latch()

        if self.m_rx_freq:
            self.tune(self.m_rx_freq)

        return

    def enable_pa(self):
        self.m_shiftreg.setbit(self.SR_TX_PA)

        self.m_shiftreg.latch()

        return

    def disable_pa(self):
        self.m_shiftreg.clearbit(self.SR_TX_PA)

        self.m_shiftreg.latch()

        return

    def set_tx_power_high(self):

        self.m_shiftreg.clearbit(self.SR_TX_POWER_HI_LO)

        self.m_shiftreg.latch()

        return

    def set_tx_power_low(self):
        self.m_shiftreg.setbit(self.SR_TX_POWER_HI_LO)

        self.m_shiftreg.latch()

        return

    def enable_audio_pa(self):
        self.m_shiftreg.setbit(self.SR_AUDIO_PA)

        self.m_shiftreg.latch()

        return

    def disable_audio_pa(self):
        self.m_shiftreg.clearbit(self.SR_AUDIO_PA)

        self.m_shiftreg.latch()

        return

    def enable_ext_alarm(self):
        self.m_shiftreg.clearbit(self.SR_EXT_ALARM)

        self.m_shiftreg.latch()

        return

    def disable_ext_alarm(self):
        self.m_shiftreg.setbit(self.SR_EXT_ALARM)

        self.m_shiftreg.latch()

        return

    def getstatus(self):

        self.m_hwif.enqueue("Z")


    def getlock(self):
        if self.m_ftdi:
            self.m_hwif.bb.ftdi_fn.ftdi_usb_purge_rx_buffer()

            lockbit=self.m_hwif.D5

            result = ((self.m_hwif.bb.port & lockbit) == lockbit)
        else:
            result = self.m_last_status & 1

        return result

    def stcharupdate(self, statstr):
        if statstr.strip():
            self.m_last_status = int(statstr)

    def initialise(self, device_id):

        if device_id == "cli":
            self.m_hwif = Cli.TelnetCLI(self)

            self.m_latch_serial_writer = SerialStreamWriter.SerialStreamWriterCLI(self.m_hwif,"C")

            self.m_synth_serial_stream_writer=SerialStreamWriter.SerialStreamWriterCLI(self.m_hwif,"S")

            self.m_ftdi = False

            self.m_status_monitor = StatusMonitor(args=(self))

            self.m_status_monitor.start()

        else:

            self.m_hwif=ft232r.ft232r(device_id = device_id)

            outputs=self.m_hwif.D0|self.m_hwif.D1|self.m_hwif.D2|self.m_hwif.D4

            self.m_hwif.initialise(outputs,0)

            self.m_latch_serial_writer = SerialStreamWriter.SerialStreamWriterFTDI(self.m_hwif, DATA=self.m_hwif.D1, CLK=self.m_hwif.D0, STB=self.m_hwif.D4)

            self.m_synth_serial_stream_writer=SerialStreamWriter.SerialStreamWriterFTDI(self.m_hwif,DATA=self.m_hwif.D1,CLK = self.m_hwif.D0, STB=self.m_hwif.D2)

            self.m_ftdi = True


        #
        # rig control
        #
        self.m_shiftreg = ShiftReg.ShiftReg(serial_writer = self.m_latch_serial_writer, nbits = 8)

        #
        # synth
        # prescaler is MC14094 (divide by 40)
        #
        self.m_synth = MC145158.MC145158(prescale_divide = 40, serial_writer=self.m_synth_serial_stream_writer, getlock=self.getlock)

        self.m_synth.set_refclk(self.m_synth_refclk)

        self.disable_tx()

        self.m_shiftreg.clearbit(self.SR_TX_AUDIO_ENABLE)

        self.enable_audio()

        return

    def enable_audio(self):
        self.m_shiftreg.setbit(self.SR_RX_AUDIO_ENABLE)

        self.m_shiftreg.latch()

        return

    def disable_audio(self):
        self.m_shiftreg.clearbit(self.SR_RX_AUDIO_ENABLE)

        self.m_shiftreg.latch()

        return

    def set_rx_freq(self,freq):

        self.m_rx_freq = freq + 21.4E6

        # FIXME
        self.tune(self.m_rx_freq)

        return

    def set_tx_freq(self,freq):

        self.m_tx_freq = freq

        return

    def set_step(self, step):
        self.set_ref_divider(self.m_synth_refclk/step)

        return

    def set_ref_divider(self,divratio):

        self.m_divratio = divratio

        self.m_synth.set_ref_divider(self.m_divratio)

        return

    def squelch_open(self):
        #
        # squelch
        #
        # squelch = self.m_hwif.D6
        #
        if self.m_ftdi:
            self.m_hwif.bb.ftdi_fn.ftdi_usb_purge_rx_buffer()

            result = not ((self.m_hwif.bb.port & self.m_hwif.D6) == self.m_hwif.D6)
        else:
            result = self.m_last_status & 2

        return result

    def locked(self):
        #
        # lock detect
        #
        # ld = self.m_hwif.D5
        #
        return self.m_synth.locked()


    def tune(self, freq):
        #
        # 14.4 Mhz reference clock
        #
        # consider moving this to init
        #
        self.m_synth.set_freq( freq );

        # 104.88726E rx == 74.1 MHz TX approx
        #self.m_synth.set_freq(104.88726E6)
        #self.m_synth.set_freq(70.4625E6+21.4E6)
        #self.m_synth.set_freq(70.18E6+21.4E6)
        #self.m_synth.set_freq(70.4875E6+21.4E6)
        #self.m_synth.set_freq(70.3875E6+21.4E6)
        #self.m_synth.set_freq(70.050E6+21.4E6)
        #self.m_synth.set_freq(70.020E6+21.4E6)
        #self.m_synth.set_freq(70.45E6+21.4E6)
        #self.m_synth.set_freq(65.38750E6+21.4E6)
        #self.m_synth.set_freq(81.9630E6+21.4E6)
        #self.m_synth.set_freq(70.01650E6+21.4E6)
        #self.m_synth.set_freq(70.4625E6)
        #self.m_synth.set_freq(10.38750E6)

        return

    def powered_on(self):

        if self.m_ftdi:
            # HACK throw away result
            self.m_hwif.bb.ftdi_fn.ftdi_usb_purge_rx_buffer()

            # HACK throw away result
            self.m_hwif.bb.port

            self.m_hwif.bb.ftdi_fn.ftdi_usb_purge_rx_buffer()

            result = ((self.m_hwif.bb.port & self.m_hwif.D3) == self.m_hwif.D3)
        else:
            result = self.m_last_status & 4

        return result


def test():
    mc = McMicro()

    mc.initialise()

    mc.power(True)
    #mc.power(False)

    mc.enable_audio()

    mc.tune()


#    mc.disable_tx()
#    mc.enable_tx()

#    mc.enable_pa()

if __name__ == "__main__":
    
    test()

    
    
