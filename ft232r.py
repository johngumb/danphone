from pylibftdi import BitBangDevice
import time

class ft232r:
    def __init__(self):
        self.bb = BitBangDevice()

        #
        # default to Hi-Z inputs
        #
        self.bb.direction = 0x00
        #self.bb.baudrate = 115200
        #self.bb.baudrate = 920000
        self.bb.baudrate = 2000000

        self.D0=1
        self.D1=2
        self.D2=4
        self.D3=8
        self.D4=16
        self.D5=32
        self.D6=64
        self.D7=128

        return

    def initialise(self, direction = 0, value = 0):

        self.bb.direction = direction

        self.setport(value)

        return

    def getport(self):
        return self.m_portval

    def setport(self,val):
        self.m_portval = val

        self.bb.port = val

        return

    def setboolbit(self,bit,val):
        if val:
            self.m_portval |= bit
        else:
            self.m_portval &= ~bit

        self.bb.port = self.m_portval

        return

    def setbits(self,bits):
        self.m_portval |= bits
        self.bb.port = self.m_portval

        return

    def clearbits(self,bits):
        self.m_portval &= ~bits
        self.bb.port = self.m_portval

        return

    def pulsebithigh(self,bit):
        self.bb.port &= ~bit
        self.bb.port |= bit
        self.bb.port &= ~bit

        return

    def pulsebitlow(self,bit,downtime=0):
        self.bb.port |= bit
        if downtime!=0:
            time.sleep(downtime)
        self.bb.port &= ~bit
        self.bb.port |= bit

        return

    def risingedge(self,bit):
        self.bb.port &= ~bit
        self.bb.port |= bit

        self.m_portval = self.bb.port

        return

    def setcbus(self,bit):
        olddir = self.bb.direction

        # flip into cbus bit bang mode with 0x20 and our value
        self.bb.ftdi_fn.ftdi_set_bitmode(0xF0|(bit&0x0F),0x20)
        #self.bb.ftdi_fn.ftdi_set_bitmode(0x00|(bit&0x0F),0x20)

        # back to normal bit bang mode
        #self.bb.ftdi_fn.ftdi_set_bitmode(olddir, 0x01)
        
        # as we were - this puts the port back into normal bit bang mode
        self.bb.direction = olddir

        return

    def pulsecbus(self,bit):
        return

        olddir = self.bb.direction

        # flip into cbus bit bang mode with 0x20 and our value
        #self.bb.ftdi_fn.ftdi_set_bitmode(0xF0,0x20)

        #self.bb.ftdi_fn.ftdi_set_bitmode(0xF0|(bit&0x0F),0x20)

        #self.bb.ftdi_fn.ftdi_set_bitmode(0xF0,0x20)

        # back to normal bit bang mode
        #self.bb.ftdi_fn.ftdi_set_bitmode(olddir, 0x01)
        
        # as we were - this puts the port back into normal bit bang mode
        self.bb.direction = olddir

        return
