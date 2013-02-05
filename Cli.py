import telnetlib
import time

class SerialCLI:
    def __init__(self):
        return

class TelnetCLI:
    def __init__(self, mcmicro):

        self.m_tn = telnetlib.Telnet("skate",2217)

        self.m_mcmicro = mcmicro

        self.m_cmdlist = []

        self.m_cmd_in_progress = False

        # debug
        self.m_current_msg = None

        self.m_last_msg = None

        return

    def enqueue(self, msg):

        #
        # optimisation
        #
        if self.m_last_msg:
            if msg==self.m_last_msg and msg[0]!="Z":
                return

        self.m_last_msg=msg

        if self.m_cmd_in_progress:
            self.m_cmdlist.append(msg)
            print "queued",msg,len(self.m_cmdlist),"elements now queued", "waiting on",self.m_current_msg

        else:
            self.m_cmd_in_progress=True
            self.send(msg)

    def resync(self):
        print "resyncing serial interface"

        startchar = 'a'

        synchar=ord(startchar)

        syncount = 0

        while syncount < 5:

            ts = 'R'+chr(synchar)+"\n"

            #print ts

            time.sleep(0.05)

            self.m_tn.write(ts)
            
            (idx, mo, txt) = self.m_tn.expect(["Y"],0.05)

            if idx == -1 and mo == None:
                print "."
                continue

            rxs = txt[-2]

            #
            # sending ahead of receiving, swallow Ys
            # till we sync
            #
            #print rxs,chr(synchar)

            if ord(rxs)<synchar:
                self.m_tn.expect(["Y"],0.1)
                syncount = 0

            if rxs == chr(synchar):
                syncount = syncount + 1
            else:
                if chr(synchar) == 'z':
                    synchar=ord(startchar)

                synchar = synchar + 1
                syncount = 0



    def xmit(self, msg):

        timeout = False

        while True:
            if timeout:
                print "retrying",msg
                time.sleep(0.1)

            self.m_tn.write(msg+"\n")

            (idx, mo, txt) = self.m_tn.expect(["K"],0.1)

            #
            # timeout?
            #
            if idx == -1 and mo == None:
                print "xmit timeout on", msg
                timeout=True

                self.resync()
            else:
                break            
            
        return ( idx, mo, txt )

    def send(self, msg):

        #print "sending",msg

        self.m_cmd_in_progress=True

        # debug
        self.m_current_msg = msg

        (idx, mo, text)=self.xmit(msg)

        if (len(self.m_cmdlist)):
            print "pending cmds, sent",msg

        if text.strip():
            self.m_mcmicro.stcharupdate(text[-2])

        while (len(self.m_cmdlist)):
            cmd = self.m_cmdlist.pop(0)
            l=len(self.m_cmdlist)
            print "dequeued",cmd, l, "elements left, sending",cmd
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
