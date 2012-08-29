import ft232r
import ShiftReg
import MC145158

# could leave tx unlocked to prevent TX PA enable
class McMicro:
    def __init__(self):
        self.SR_AUDIO_PA=0x80
        self.SR_POWER=0x40 # pin 5
        self.SR_TX_RX=0x20 # pin 6, ensure PA stays off initially
        self.SR_TX_POWER_HI_LO=0x10
        self.SR_TX_PA=0x08
        self.SR_PA=0x04
        self.SR_TX_AUDIO_ENABLE=0x02
        self.SR_RX_AUDIO_ENABLE=0x01

        #0x80 pin 4
        #0x10 pin 7 hi/lo power ??

        #0x01 pin 11
        #0x02 pin 12
        #0x04 pin 13
        #0x08 pin 14
        
        self.m_synth_refclk = 14.4E6

        return

    def power(self, val):
        if val:
            self.m_shiftreg.setbit(self.SR_POWER)
        else:
            self.m_shiftreg.clearbit(self.SR_POWER)

        self.m_shiftreg.latch()

        return

    def enable_tx(self):
        self.m_shiftreg.setbit(self.SR_TX_RX|self.SR_TX_AUDIO_ENABLE)

        self.m_shiftreg.latch()

        return

    def enable_pa(self):
        self.SR_PA=0x04
#        self.SR_PA=0x00
#        self.SR_PA=0x01
#        self.m_shiftreg.setbit(self.SR_PA)
        self.m_shiftreg.setbit(self.SR_PA)


        self.m_shiftreg.latch()

        return

    def disable_tx(self):
        self.m_shiftreg.clearbit(self.SR_TX_RX)

        self.m_shiftreg.latch()

        return

    def initialise(self):
        self.m_hwif=ft232r.ft232r(device_id="MCVEC40K")
        
        outputs=self.m_hwif.D0|self.m_hwif.D1|self.m_hwif.D2|self.m_hwif.D3

        self.m_hwif.initialise(outputs,0)

        #
        # rig control
        #
        self.m_shiftreg = ShiftReg.ShiftReg(self.m_hwif, self.m_hwif.D0, \
                                         self.m_hwif.D1, self.m_hwif.D3, 8)

        #
        # synth
        # prescaler is MC14094 (divide by 40)
        #
        self.m_synth = MC145158.MC145158(self.m_hwif, 40, self.m_hwif.D1, \
                                         self.m_hwif.D0, self.m_hwif.D2)

        self.m_shiftreg.clearbit(self.SR_TX_AUDIO_ENABLE)

        return

    def enable_audio(self):
        self.m_shiftreg.setbit(self.SR_RX_AUDIO_ENABLE)

        self.m_shiftreg.latch()

        return
        
    def tune(self):
        #
        # 14.4 Mhz reference clock
        #
        self.m_synth.set_refclk(self.m_synth_refclk)

        #
        # nail up 6.25 Khz steps for now
        #
        self.m_synth.set_ref_divider(self.m_synth_refclk/6.25E3)

        # 104.88726E rx == 74.1 MHz TX approx
        #self.m_synth.set_freq(104.88726E6)
        #self.m_synth.set_freq(70.4625E6+21.4E6)
        #self.m_synth.set_freq(70.18E6+21.4E6)
        #self.m_synth.set_freq(70.4875E6+21.4E6)
        self.m_synth.set_freq(70.3875E6+21.4E6)
        #self.m_synth.set_freq(70.050E6+21.4E6)
        #self.m_synth.set_freq(65.38750E6+21.4E6)
        #self.m_synth.set_freq(81.9630E6+21.4E6)
        #self.m_synth.set_freq(70.01650E6+21.4E6)
        #self.m_synth.set_freq(70.4625E6)
        #self.m_synth.set_freq(10.38750E6)

        return

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

    
    
