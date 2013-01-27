import telnetlib

class SerialCLI:
    def __init__(self):
        return

class TelnetCLI:
    def __init__(self):
        
        self.m_tn = telnetlib.Telnet("skate",2217)

    def send(self, msg):
        print msg
        self.m_tn.write(msg)

    def enter(self):
        self.m_tn.write("\n")
