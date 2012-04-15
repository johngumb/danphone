import ft232r
import ShiftReg

class McMicro:
    def __init__(self):
        self.SR_POWER=0x40

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

        return

def test():
    mc = McMicro()

    mc.initialise()

    mc.power(True)

if __name__ == "__main__":
    
    test()

    
    
