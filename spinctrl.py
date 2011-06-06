#!/usr/bin/python

# spinctrl.py

import os

import wx
import ledthing
#from agw import floatspin
import wx.lib.agw.floatspin as FS

import ft232r
import DanPhone
import sys

ID_SPIN_1=wx.NewId()
ID_SPIN_2=wx.NewId()
ID_LED_1=wx.NewId()
ID_LED_2=wx.NewId()
ID_LED_SQUELCH=wx.NewId()
ID_TEXT_1=wx.NewId()
ID_TEXT_2=wx.NewId()
ID_BUTTON_7=wx.NewId()
ID_BUTTON_MONITOR=wx.NewId()

class StatusLEDtimer(wx.Timer):
    def __init__(self,target,dur=500):
        wx.Timer.__init__(self)
        self.target = target
        self.Start(dur)
        return

    def Notify(self):
        """Called every timer interval"""

        if self.target.m_rig.tx_enabled():
            if self.target.m_rig.tx_locked():
                self.target.m_led1.SetState(2)
            else:
                self.target.m_led1.SetState(0)
        else:
            self.target.m_led1.SetState(3)

        if self.target.m_rig.rx_locked():
            self.target.m_led2.SetState(2)
        else:
            self.target.m_led2.SetState(0)

        sopen=self.target.m_rig.squelch_open()
 
        lastopen=True
        lastclosed=True
#        lastopen  = self.target.m_sopen_last_time[0] and self.target.m_sopen_last_time[1]
#        lastclosed  = (not self.target.m_sopen_last_time[0]) and (not self.target.m_sopen_last_time[1])
        lastopen  = self.target.m_sopen_last_time[0]
        lastclosed  = (not self.target.m_sopen_last_time[0])
            
        if sopen and lastopen:
            self.target.m_squelch_led.SetState(2)
            if not self.target.m_monitor_button.GetValue():
                self.target.unmute()

        if (not sopen) and lastclosed:
            self.target.m_squelch_led.SetState(0)
            if not self.target.m_monitor_button.GetValue():
                self.target.mute()

        self.target.m_sopen_last_time[1] = self.target.m_sopen_last_time[0]

        self.target.m_sopen_last_time[0] = sopen


        wx.WakeUpIdle()

        return

class MyFrame(wx.Frame):
    def __init__(self, *args, **kwds):
        # begin wxGlade: MyFrame.__init__
        kwds["style"] = wx.DEFAULT_FRAME_STYLE
        wx.Frame.__init__(self, *args, **kwds)

        self.m_rig = DanPhone.DanPhone()

        self.m_step = 6.25E3

        self.m_min_freq=65.9E6

        self.m_max_freq=77E6

        self.m_rig.set_step(self.m_step)

        self.m_rig.init_rx()

#        self.m_rig.enable_tx()

        self.m_led1=ledthing.LED(self,ID_LED_1)

        self.m_spin_ctrl_1 = FS.FloatSpin(self, ID_SPIN_1)
        
        self.m_spin_ctrl_2 = FS.FloatSpin(self, ID_SPIN_2)

        self.m_led2=ledthing.LED(self,ID_LED_2)

        self.m_squelch_led=ledthing.LED(self,ID_LED_SQUELCH)

        steps=["Auto", "3.2 KHz","4","5","6.25","8","10","12.5"]

        self.m_step_selected = "6.25"

        self.m_step_combo = wx.ComboBox(self, -1, self.m_step_selected, choices=steps)

        self.m_digits = 5
        print dir(self.m_spin_ctrl_2 )
        for f in [self.m_spin_ctrl_1, self.m_spin_ctrl_2]:
            f.SetFormat("%F")
            f.SetDigits(self.m_digits)
            self.m_freq=70.3875E6
            f.SetDefaultValue( self.m_freq /1E6)
            f.SetValue( self.m_freq / 1E6)
            f.SetToDefaultValue()
            f.SetSnapToTicks(True)
            f.SyncSpinToText()
            f.Bind(FS.EVT_FLOATSPIN, self.OnFloatSpin)
            f._increment=self.m_step/1E6
            f.SetRange(self.m_min_freq/1E6,self.m_max_freq/1E6)

        self.m_rig.set_rx_freq(self.m_spin_ctrl_2.GetDefaultValue()*1E6)
        self.m_rig.set_tx_freq(self.m_spin_ctrl_2.GetDefaultValue()*1E6)

        self.button_7 = wx.ToggleButton(self, ID_BUTTON_7, "Tx")

        self.m_monitor_button = wx.ToggleButton(self, ID_BUTTON_MONITOR, "Mon")

        self.status_led_timer=StatusLEDtimer(self,400)

        #self.__set_properties()

        wx.EVT_TOGGLEBUTTON(self,ID_BUTTON_7,self.onButtonTx)

        self.m_mute = False
        wx.EVT_TOGGLEBUTTON(self,ID_BUTTON_MONITOR,self.onButtonMonitor)

        # watch freq step here

        self.__do_layout()

        self.onButtonMonitor(None)

        self.m_sopen_last_time = {}

        self.m_sopen_last_time[0] = False

        self.m_sopen_last_time[1] = False

        return

    def onButtonTx(self,event):
        if self.button_7.GetValue():
            self.m_rig.enable_tx()
        else:
            self.m_rig.disable_tx()

        return

    def onButtonMonitor(self,event):
        if self.m_monitor_button.GetValue():
            self.unmute()

        return

    def OnFloatSpin(self,event):
        floatspin = event.GetEventObject()

        v = floatspin.GetValue() * 1E6

        print v

        if v < self.m_freq:
            v = v - 10
        else:
            v = v + 10

        x = v / self.m_step

        print x
        print round(x)

        nv = round(x) * self.m_step

        print nv

        floatspin.SetValue(round(nv/1E6,self.m_digits))

        self.m_freq=floatspin.GetValue()*1E6

        self.m_rig.set_rx_freq(self.m_freq)
        self.m_rig.set_tx_freq(self.m_freq)

        return

    def OnFStextEnter(self,event):
        floatspin = event.GetEventObject()


        self.OnFloatSpin(event)

#        print type(floatspin.GetValue())

#        self.m_rig.set_rx_freq(float(floatspin.GetValue())*1E6)
#        self.m_rig.set_tx_freq(float(floatspin.GetValue())*1E6)

        return

    def OnStepSelected(self,event=None):
        print "step",event.GetString()

        khz_val=event.GetString().split()[0]
        print khz_val

        self.m_step = float(khz_val)*1E3

        #
        # snap to new step
        #
        x=self.m_freq / self.m_step

        nv = round(x) * self.m_step

        self.m_spin_ctrl_1.SetValue(round(nv/1E6,self.m_digits))

        self.m_spin_ctrl_2.SetValue(round(nv/1E6,self.m_digits))

        self.m_rig.set_step(self.m_step)

        #
        # don't know why these are necessary
        #
        self.m_rig.init_tx()

        self.m_rig.init_rx()


        self.m_freq = nv

        self.m_rig.set_rx_freq(self.m_freq)
        self.m_rig.set_tx_freq(self.m_freq)

        return

    def mute(self):
        if not self.m_mute:
            os.system("jack_disconnect cobbler:from_slave_2 system:playback_1")
            os.system("jack_disconnect cobbler:from_slave_2 system:playback_2")
            self.m_mute = True

        return

    def unmute(self):
        if self.m_mute:
            os.system("jack_connect cobbler:from_slave_2 system:playback_1")
            os.system("jack_connect cobbler:from_slave_2 system:playback_2")
            self.m_mute = False
        
        return


    def __do_layout(self):
        sizer_1 = wx.BoxSizer(wx.HORIZONTAL)

        sizer_1.Add(self.m_spin_ctrl_1 , 0, wx.ADJUST_MINSIZE, 0)
        sizer_1.Add(self.m_led1, 0, wx.ADJUST_MINSIZE, 0)

        sizer_1.Add(self.button_7, 0, wx.ADJUST_MINSIZE, 0)
        sizer_1.Add(self.m_monitor_button, 0, wx.ADJUST_MINSIZE, 0)
        
        sizer_1.Add(self.m_spin_ctrl_2 , 0, wx.ADJUST_MINSIZE, 0)
        sizer_1.Add(self.m_led2, 0, wx.ADJUST_MINSIZE, 0)

        self.m_step_combo.SetStringSelection("6.25")
                                                                                
        self.Bind(wx.EVT_COMBOBOX, self.OnStepSelected, self.m_step_combo) 

        sizer_1.Add(self.m_step_combo, 0, wx.ADJUST_MINSIZE, 0)

        sizer_1.Add(self.m_squelch_led, 0, wx.ADJUST_MINSIZE, 0)

        self.SetAutoLayout(True)
        self.SetSizer(sizer_1)
        sizer_1.Fit(self)
        sizer_1.SetSizeHints(self)
        self.Layout()

class MyApp(wx.App):
    def OnInit(self):

        frame = MyFrame(None, -1, "testradio")
        frame.Show(True)
        self.SetTopWindow(frame)
        return True

if __name__=="__main__":
    try:
        os.system("lsmod | grep -q ftdi_sio && rmmod ftdi_sio")
        app = MyApp(clearSigInt=True)
        app.MainLoop()
        mute()        

    except KeyboardInterrupt:
        mute()
        sys.exit(1)

