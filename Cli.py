import telnetlib

class SerialCLI:
    def __init__(self):
        return

class TelnetCLI:
    def __init__(self, mcmicro):

        self.m_tn = telnetlib.Telnet("skate",2217)

        self.m_mcmicro = mcmicro

        self.m_cmdlist = []

        self.m_cmd_in_progress = False

        return

    def enqueue(self, msg):

        if self.m_cmd_in_progress:
            self.m_cmdlist.append(msg)
            print "queued",msg,len(self.m_cmdlist),"elements now queued"
        else:
            self.m_cmd_in_progress=True
            self.send(msg)

    def xmit(self, msg):
        self.m_tn.write(msg+"\n")

        return self.m_tn.expect(["K"])

    def send(self, msg):

        #print "sending",msg

        self.m_cmd_in_progress=True

        (idx, mo, text)=self.xmit(msg)

        #print text

        self.m_mcmicro.stcharupdate(text[-2])

        while (len(self.m_cmdlist)):
            cmd = self.m_cmdlist.pop(0)
            l=len(self.m_cmdlist)
            print "dequeued",cmd, l, "elements left"
            self.send(cmd)

        # system should notify in case of state change
        #self.xmit("Y")

        self.m_cmd_in_progress=False


    def read_until(self, expected):
        #return self.m_tn.read_until(expected)
        #return self.m_tn.expect(["Z"])
        return self.m_tn.read_very_eager()

    def cmd_in_progress(self):
        return self.m_cmd_in_progress
