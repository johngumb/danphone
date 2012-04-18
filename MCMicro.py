import ft232r
import ShiftReg
import MC145158

class McMicro:
    def __init__(self):
        self.SR_POWER=0x40

        self.m_synth_refclk = 14.4E6

        return

    def power(self, val):
        if val:
            self.m_shiftreg.setbit(self.SR_POWER)
        else:
            self.m_shiftreg.clearbit(self.SR_POWER)

        self.m_shiftreg.latch()

    def initialise(self):
        self.m_hwif=ft232r.ft232r()
        
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

        self.m_synth.set_freq(103.88726E6)

        return

def test():
    mc = McMicro()

    mc.initialise()

    mc.power(True)

    mc.tune()

if __name__ == "__main__":
    
    test()

    
    
