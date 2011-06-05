class PhyInterface:
    def __init__(self):
        return

class SerialPhyInterface(PhyInterface):
    def __init__(self):
        return

    #
    # pin assignments
    #
    def assign_data(self, data_line):
        self.m_data = data_line
        return

    def assign_strobe(self, strobe_line):
        self.m_strobe = strobe_line
        return

    def assign_clock(self, clock_line):
        self.m_clock = clock_line
        return


class DataLink:
    def __init__(self,phy):
        return

    def send_data(self,data):
        return

class DualModulusPrescaler:
    def __init__(self,p,datalink):
        self.m_p = p
        self.m_datalink = datalink
        return

    def set_prescaler(self):
        return

    # may be internal
    def set_a(self,a):
        self.m_a = a
        return

    # may be internal
    def set_n(self,n):
        self.m_n = n
        return

    def set_ref_divider(self, r):
        self.m_r = r
        return

    def get_ref_divider(self):
        return self.m_r

    def get_division_ratio(self):
        v = (self.m_p * self.m_n) + self.m_a
        return v

class Osc:
    def __init__(self, ref_freq, prescaler):
        self.m_ref = ref_freq
        self.m_prescaler = prescaler
        return

    def get_freq(self):
#        print self.m_prescaler.get_division_ratio()
        return (self.m_ref/self.m_prescaler.get_ref_divider()) * self.m_prescaler.get_division_ratio()

class Danphone:
    def __init__(self,datalink):

        dmps = DualModulusPrescaler(32,None)

        dmps.set_ref_divider(2048)

        for i in range(128):
            dmps.set_a(i)

            dmps.set_n(456)

            osc = Osc(12.8e6,dmps)

            print osc.get_freq()/1000000

        return

if __name__=="__main__":
    d = Danphone(None)
