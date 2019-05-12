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
# along with OpenPMR.  If not, see <http://www.gnu.org/licenses/>.

import os
import time
import threading

import radiosim
import ctcss_helper

import ft232r
import ShiftReg
import MC145158
import SerialStreamWriter
import Cli

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

            mcmicro.m_hwif.update_power_present()

            #
            # no need to queue a status command
            # if there is already one in progress
            # as status comes back on all commands
            #
            if not mcmicro.m_hwif.cmd_in_progress() and mcmicro.status_polling_enabled():
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

        # MC14094 shift register pin usage
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

        self.m_inhibit_audio_pa = False

        self.m_ctcss_fudge = 1.0

        # consider what the default should be
        self.m_power_supply_present = True

        self.m_refosc_count = 0

        self.m_refosc_init_boundary = 3

        self.m_tx_on = False

        self.m_temperature_mv = 0

        self.m_last_temperature = 0.0

        self.m_status_polling_enabled = True

        self.m_status_count = 0

        self.m_audio_pa_enabled = False

        return

    def __del__(self):
        self.m_request_thread_exit = True

        self.disable_tx()

        self.setpower(False)

        return

    def disable_status_polling(self):
        '''
        prevent 'Z' command getting in the way of FT8 DAC commands
        '''
        self.m_status_polling_enabled = False

    def enable_status_polling(self):
        '''
        prevent 'Z' command getting in the way of FT8 DAC commands
        '''
        self.m_status_polling_enabled = True

    def status_polling_enabled(self):
        return self.m_status_polling_enabled

    def set_ctcss_fudge(self, val):
        self.m_ctcss_fudge = val

    def setpower(self, val):
        #print "setpower", val
        if val:
            self.m_shiftreg.setbit(self.SR_POWER)
        else:
            self.m_shiftreg.clearbit(self.SR_POWER)

        self.m_shiftreg.latch()

        self.m_requested_power_state = val

        return

    def getpower(self):
        return self.m_requested_power_state

#        elif self.m_tx_freq in [145.1375E6]: # AL
    def enable_tx(self, ctcss = 0, enable_tx_audio = True):
        #print "enable_tx"

        if self.m_tx_freq in [50.05E6, 50.016E6]:
            return

        self.tune(self.m_tx_freq)

        if enable_tx_audio:
            self.m_shiftreg.setbit(self.SR_TX_RX|self.SR_TX_AUDIO_ENABLE)
        else:
            self.m_shiftreg.setbit(self.SR_TX_RX)

        self.m_shiftreg.latch()

        if enable_tx_audio:
            ctcss = ctcss_helper.get_ctcss(self.m_tx_freq)

            self.set_ctcss(ctcss)

        self.m_tx_on = True

        return

    def disable_tx(self):
        #print "disable_tx"

        self.m_shiftreg.clearbit(self.SR_TX_RX|self.SR_TX_AUDIO_ENABLE)

        self.m_shiftreg.latch()

        if self.m_rx_freq:
            self.tune(self.m_rx_freq)

        self.set_ctcss(0)

        self.m_tx_on = False

        return

    def enable_pa(self):
        #print "enable_pa"

        self.m_shiftreg.setbit(self.SR_TX_PA)

        self.m_shiftreg.latch()

        return

    def disable_pa(self):
        #print "disable_pa"

        self.m_shiftreg.clearbit(self.SR_TX_PA)

        self.m_shiftreg.latch()

        return

    def set_tx_power_high(self):
        if self.m_hwif.server()=="skate":
            self.m_hwif.enqueue("P9C")
        elif self.m_hwif.server()=="rudd":
            self.m_hwif.enqueue("PB0")
        else:
            self.m_shiftreg.clearbit(self.SR_TX_POWER_HI_LO)

            self.m_shiftreg.latch()

        return

    def set_tx_power_low(self):
        if self.m_hwif.server()=="skate":
            self.m_hwif.enqueue("P60")
        if self.m_hwif.server()=="rudd":
            self.m_hwif.enqueue("P28")
        else:
            self.m_shiftreg.setbit(self.SR_TX_POWER_HI_LO)

            self.m_shiftreg.latch()

        return

    def inhibit_audio_pa(self):
        return os.path.exists("/tmp/inhibit_audio_pa") or self.m_inhibit_audio_pa

    def enable_audio_pa(self):
        #print "enable_audio_pa"

        if not self.inhibit_audio_pa():
            if not self.m_audio_pa_enabled:
                self.m_shiftreg.setbit(self.SR_AUDIO_PA)

                self.m_shiftreg.latch()

                self.m_audio_pa_enabled = True
        return

    def disable_audio_pa(self):
        #print "disable_audio_pa"

        if self.m_audio_pa_enabled:
            self.m_shiftreg.clearbit(self.SR_AUDIO_PA)

            self.m_shiftreg.latch()

            self.m_audio_pa_enabled = False

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
        if self.m_status_count == 10:
            # get temperature periodically
            self.m_hwif.enqueue("H")
        else:
            self.m_hwif.enqueue("Z")

        self.m_status_count += 1

        if self.m_status_count == 50:
            self.m_status_count = 0

    def getlock(self):
        if self.m_ftdi:
            self.m_hwif.bb.ftdi_fn.ftdi_usb_purge_rx_buffer()

            lockbit=self.m_hwif.D5

            result = ((self.m_hwif.bb.port & lockbit) == lockbit)
        else:
            result = self.m_last_status & 1

        return result

    def get_tx_pa_state(self):
        return (self.m_last_status & 8) != 0

    def stcharupdate(self, statstr):
        '''
        status update to mc micro - status string must be stripped
        '''
        try:
            self.m_last_status = int(statstr[-1],16)
            if len(statstr) > 1:
                self.m_temperature_mv = int(statstr[:-1],16)

        except ValueError:
            print "status ignored - not integer",statstr

    def set_power_supply_state(self, power_supply_state):
        self.m_power_supply_present = power_supply_state
        
    def initialise(self, device_id):
        '''
        Initialisation which shouldn't touch the hardware, just setup software
        '''

        (devtype, devaddr) = device_id

        if devtype == "cli":
            self.m_hwif = Cli.TelnetCLI(self, devaddr)

            self.m_latch_serial_writer = SerialStreamWriter.SerialStreamWriterCLI(self.m_hwif,"C")

            self.m_synth_serial_stream_writer=SerialStreamWriter.SerialStreamWriterCLI(self.m_hwif,"S")

            self.m_ftdi = False

            self.m_status_monitor = StatusMonitor(args=(self))

            self.m_status_monitor.start()

        else:

            self.m_hwif=ft232r.ft232r(device_id = devaddr)

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

        self.m_requested_rx_freq = freq

        if freq < 100.0E6:
            self.m_rx_freq = freq + 21.4E6
        else:
            self.m_rx_freq = freq - 21.4E6

        # FIXME
        self.tune(self.m_rx_freq)

        return

    def get_rx_freq(self):

        return self.m_requested_rx_freq

    def set_ctcss(self, tone):

        if tone:
            #
            # 74LS393 divides by 128
            # wiring: GND: 2, 12, 7
            #         connected: 6-13
            #
            fudge = self.m_ctcss_fudge

            byteval=int(round(24.5E6/(12*fudge))/(tone*128))
        else:
            #
            # CTCSS off
            #
            byteval=0

        if not self.m_ftdi:
            self.m_hwif.enqueue("T%02X" % byteval)

        return

    def set_tx_freq(self,freq):

        freq = round(freq)

        if (freq >= 50.72E6) and (freq <= 50.88E6):
            tx_offset = 5E5
        elif (freq >= 145.6E6) and (freq <= 145.8E6):
            tx_offset = -6E5
        else:
            tx_offset = 0

        self.m_tx_freq = freq + tx_offset

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

        #
        # ignore any false readings
        # if there's no PSU
        #
        if not self.power_supply_present():
            result = 0

        #
        # hack - really need somewhere to initialise stuff when
        # RF board comes up
        # Initialise reference oscillator DAC
        #
        if self.m_refosc_count > self.m_refosc_init_boundary and not result:
            # deal with power going away
            self.m_refosc_count = 0

        if self.m_refosc_count == self.m_refosc_init_boundary and result:
            # Need Rx audio
            self.enable_audio()

            # Don't come up in transmit mode
            self.disable_tx()

            if not self.m_ftdi and self.m_hwif.server()=="skate":
                # 14.4MHz on ref osc
                #self.m_hwif.enqueue("E2C56")

                # 50MHz reception measured at 71.4MHz LO
                #val=0xC3E
                val=0xC300
                calfile="/home/john/6mcal"
                if os.path.exists(calfile):
                    with open(calfile) as caldata:
                        val+=int(caldata.read())
                print "sending","M%04X" % val
                self.m_hwif.enqueue("M%X" % val)

                # squelch pot
                self.m_hwif.enqueue("QE0")

            if not self.m_ftdi and self.m_hwif.server()=="rudd":
                # 14.4MHz on ref osc

                val=0xBF00
                calfile="/home/john/2mcal"
                if os.path.exists(calfile):
                    with open(calfile) as caldata:
                        val+=int(caldata.read())
                print "sending","M%04X" % val
                self.m_hwif.enqueue("M%04X" % val)

            if not self.m_ftdi and self.m_hwif.server()=="dab":
                # 14.4MHz on ref osc

                val=0xBF80
                calfile="/home/john/4mcal"
                if os.path.exists(calfile):
                    with open(calfile) as caldata:
                        val+=int(caldata.read())
                print "sending","M%04X" % val
                self.m_hwif.enqueue("M%04X" % val)
                self.m_hwif.enqueue("pin15off")

            self.m_refosc_count += 1
        elif self.m_refosc_count == self.m_refosc_init_boundary and not result:
            print "boundary hit but no power"
        else:
            if self.m_refosc_count < self.m_refosc_init_boundary and result:
                self.m_refosc_count += 1

        return result

    def execute_rig_cmd(self, cmd):
        print "execute_rig_cmd: Executing", cmd
        self.m_hwif.enqueue(cmd)

    def power_supply_present(self):

        return self.m_power_supply_present

    def set_pin1(self, val):
        if val:
            self.m_hwif.enqueue("pin1on")
        else:
            self.m_hwif.enqueue("pin1off")

    def take_temperature(self):
        # silabs-c8051-temperature.pdf AN103
        current_temperature = (776 - self.m_temperature_mv)/2.86

        if self.m_temperature_mv == 0:
            changed = False
        else:
            changed = (current_temperature != self.m_last_temperature)

        self.m_last_temperature = current_temperature

        return (changed, current_temperature)

    def command_duration(self):
        self.disable_status_polling()

        cmd_count = 40

        result={}

        for j in [0,1]:

            if j == 0:
                self.m_hwif.enqueue("EA19F")
            else:
                self.m_hwif.enqueue("E0000")

            i = 0
            start_time=time.time()

            while i < cmd_count:
                self.m_hwif.enqueue("D2C3E")
                i+=1

            end_time=time.time()

            result[j]=end_time - start_time

        self.enable_status_polling()

        return (result[0]-result[1])/cmd_count
        
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

    
    
