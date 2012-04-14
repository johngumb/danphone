import ft232r
import ShiftReg

class McMicro:
    def __init__(self):
        return

    def test(self):
        self.m_hwif=ft232r.ft232r()
        
        outputs=self.m_hwif.D0|self.m_hwif.D1|self.m_hwif.D2|self.m_hwif.D3

        self.m_hwif.initialise(outputs,0)

        #
        # peripheral interface
        #
        self.m_shiftreg = ShiftReg.ShiftReg(self.m_hwif, self.m_hwif.D0, \
                                         self.m_hwif.D1, self.m_hwif.D4, 8)

        self.m_shiftreg.setbit(128)
        
        self.m_shiftreg.latch()

        return

def test():
    mc = McMicro()

    mc.test()

if __name__ == "__main__":
    
    test()

    
    
