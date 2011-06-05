import sys

class DualModulusPrescaler:
    def __init__(self,p):
        self.m_p = p
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

    def calc_a(self):
        return

    def calc_n(self):
        return

    def get_counter_params(self,freq):
        x = freq * self.m_prescaler.get_ref_divider()/self.m_ref

        n = int(x/32)

        a = int(round(x-n*32))

        encoded = (n<<7) + a

        return (n, a, encoded)

    def set_freq(self,freq):
        return

class Danphone:
    def __init__(self,datalink):

        dmps = DualModulusPrescaler(32)

        #dmps.set_ref_divider(2048)

        dmps.set_ref_divider(1088)

        osc = Osc(12.8e6,dmps)

        print osc.get_counter_params(70.3529E6)

        sys.exit(0)

        for i in range(128):
            dmps.set_a(i)

            dmps.set_n(456)

            osc = Osc(12.8e6,dmps)

            print osc.get_freq()/1000000

        return

if __name__=="__main__":
    d = Danphone(None)
