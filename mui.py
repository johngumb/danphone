#!/usr/bin/python

# spinctrl.py

import os
import time

import wx
import ledthing
#from agw import floatspin
import wx.lib.agw.floatspin as FS

import McMicro
import sys

ID_SPIN_1=wx.NewId()
ID_SPIN_2=wx.NewId()
ID_LED_2=wx.NewId()
ID_LED_SQUELCH=wx.NewId()
ID_TEXT_1=wx.NewId()
ID_TEXT_2=wx.NewId()
ID_BUTTON_TX_RX=wx.NewId()
ID_BUTTON_PA=wx.NewId()
ID_BUTTON_TX_POWER_LEVEL=wx.NewId()
ID_BUTTON_MONITOR=wx.NewId()
ID_BUTTON_TX=wx.NewId()
ID_SPIN_SQUELCH_LEVEL=wx.NewId()
ID_BUTTON_MUTE=wx.NewId()
ID_BUTTON_MUTE_SKYPE=wx.NewId()
ID_BUTTON_ON_OFF=wx.NewId()
ID_BUTTON_AUDIO_PA=wx.NewId()
ID_BUTTON_EXT_ALARM=wx.NewId()

MUTED = False

# TODO fix initial mute state
# TODO radio might start with signal present.
def mute():
    global MUTED
    if not MUTED:
        os.system("jack_disconnect dab:from_slave_2 system:playback_1")
        os.system("jack_disconnect dab:from_slave_2 system:playback_2")
        MUTED = True
    return

def unmute():
    global MUTED
    if MUTED:
        os.system("jack_connect dab:from_slave_2 system:playback_1")
        os.system("jack_connect dab:from_slave_2 system:playback_2")
        MUTED = False
    return

class TxTimer(wx.Timer):
    def __init__(self,target):
        wx.Timer.__init__(self)
        self.target = target
        return
    
    def Notify(self):
        if self.target.m_tx_button.GetValue():
            print "tx timeout"
            self.target.m_tx_button.SetValue(False)
            self.target.onButtonTransmit(None)

        wx.WakeUpIdle()

        return

class StatusLEDtimer(wx.Timer):
    def __init__(self,target,dur=500):
        wx.Timer.__init__(self)
        self.target = target
        self.m_squelch_sample = False

        self.Start(dur)
        return

    def Notify(self):
        """Called every timer interval"""
        if not self.target.m_powered_on:
            self.target.m_led2.SetState(3)
            self.target.m_squelch_led.SetState(3)
            return

        if self.target.m_rig.locked():
            self.target.m_led2.SetState(2)
        else:
            self.target.m_led2.SetState(0)

        sopen=self.target.m_rig.squelch_open()

        if not self.m_squelch_sample:
            if sopen:
                self.target.m_squelch_led.SetState(2)
                if not self.target.m_monitor_button.GetValue():
                    if not self.target.m_stay_muted:
                        unmute()
            else:
                self.target.m_squelch_led.SetState(0)
                if not self.target.m_monitor_button.GetValue():
                    mute()
        else:
            lastopen = True
            samples_to_check = 3
            for i in range(samples_to_check):
                if self.target.m_sopen_last_time.has_key(i):
                    lastopen  = lastopen and self.target.m_sopen_last_time[i]

#        lastclosed  = (not self.target.m_sopen_last_time[0]) and (not self.target.m_sopen_last_time[1])
#        lastopen  = self.target.m_sopen_last_time[0]
            lastclosed  = (not self.target.m_sopen_last_time[0])
            
            if sopen and lastopen:
                self.target.m_squelch_led.SetState(2)
                if not self.target.m_monitor_button.GetValue():
                    if not self.target.m_stay_muted:
                        unmute()

            if (not sopen) and lastclosed:
                self.target.m_squelch_led.SetState(0)
                if not self.target.m_monitor_button.GetValue():
                    mute()

#        self.target.m_sopen_last_time[3] = self.target.m_sopen_last_time[2]

#        self.target.m_sopen_last_time[2] = self.target.m_sopen_last_time[1]

#        self.target.m_sopen_last_time[1] = self.target.m_sopen_last_time[0]

#        self.target.m_sopen_last_time[0] = sopen

            for i in range(samples_to_check-1, 0, -1):
                if self.target.m_sopen_last_time.has_key(i-1):
                    self.target.m_sopen_last_time[i] = self.target.m_sopen_last_time[i-1]
            self.target.m_sopen_last_time[0] = sopen

        #print self.target.m_rig.powered_on()

        wx.WakeUpIdle()

        return

class MyFrame(wx.Frame):
    def __init__(self, *args, **kwds):
        # begin wxGlade: MyFrame.__init__
        kwds["style"] = wx.DEFAULT_FRAME_STYLE
        wx.Frame.__init__(self, *args, **kwds)

        self.m_rig = McMicro.McMicro()

        self.m_step = 6.25E3

        self.m_min_freq=65.9E6

        self.m_max_freq=77E6

        self.m_rig.set_step(self.m_step)

        self.m_rig.initialise()

        self.m_spin_ctrl_1 = FS.FloatSpin(self, ID_SPIN_1)
        
        self.m_spin_ctrl_2 = FS.FloatSpin(self, ID_SPIN_2)

        self.m_led2=ledthing.LED(self,ID_LED_2)

        self.m_squelch_led=ledthing.LED(self,ID_LED_SQUELCH)

        steps=["Auto", "3.2 KHz","4","5","6.25","8","10","12.5"]

        self.m_step_selected = "6.25"

        self.m_spin_ctrl_squelch_level = FS.FloatSpin(self, ID_SPIN_2)

        self.m_step_combo = wx.ComboBox(self, -1, self.m_step_selected, choices=steps)

        self.m_digits = 5
#        print dir(self.m_spin_ctrl_2 )
        for f in [self.m_spin_ctrl_1, self.m_spin_ctrl_2]:
            f.SetFormat("%F")
            f.SetDigits(self.m_digits)
            self.m_freq=70.45E6
            f.SetDefaultValue( self.m_freq /1E6)
            f.SetValue( self.m_freq / 1E6)
            f.SetToDefaultValue()
            f.SetSnapToTicks(True)
            f.SyncSpinToText()
            f.Bind(FS.EVT_FLOATSPIN, self.OnFloatSpin)
            f._increment=self.m_step/1E6
            f.SetRange(self.m_min_freq/1E6,self.m_max_freq/1E6)

        self.m_squelch_level_value = 500
        self.m_spin_ctrl_squelch_level.SetFormat("%F")
        self.m_spin_ctrl_squelch_level.SetDigits(0)
        self.m_spin_ctrl_squelch_level.SetDefaultValue(5)
        self.m_spin_ctrl_squelch_level.SetToDefaultValue()
        self.m_spin_ctrl_squelch_level.SetRange(1,2000)
        self.m_spin_ctrl_squelch_level.SetValue(self.m_squelch_level_value)
        self.m_spin_ctrl_squelch_level.Bind(FS.EVT_FLOATSPIN, self.OnSquelchFloatSpin)
        self.m_rig.set_rx_freq(self.m_spin_ctrl_2.GetDefaultValue()*1E6)
        self.m_rig.set_tx_freq(self.m_spin_ctrl_2.GetDefaultValue()*1E6)

        self.m_tx_rx = wx.ToggleButton(self, ID_BUTTON_TX_RX, "Tx/Rx")

        self.m_button_pa = wx.ToggleButton(self, ID_BUTTON_PA, "PA")

        self.m_button_tx_power_level = wx.ToggleButton(self, ID_BUTTON_TX_POWER_LEVEL, "QRO")

        self.m_monitor_button = wx.ToggleButton(self, ID_BUTTON_MONITOR, "Mon")

        self.m_tx_button = wx.ToggleButton(self, ID_BUTTON_TX, "Tx")

        self.m_mute_button = wx.ToggleButton(self, ID_BUTTON_MUTE, "Mute")

        self.m_mute_button_skype = wx.ToggleButton(self, ID_BUTTON_MUTE_SKYPE, "MuteSkype")

        self.m_on_off_button = wx.ToggleButton(self, ID_BUTTON_ON_OFF, "On/Off")

        self.m_audio_pa_button = wx.ToggleButton(self, ID_BUTTON_AUDIO_PA, "SPKR")

        self.m_ext_alarm_button = wx.ToggleButton(self, ID_BUTTON_EXT_ALARM, "AUX")

        self.status_led_timer=StatusLEDtimer(self,400)

        #self.__set_properties()

        wx.EVT_TOGGLEBUTTON(self,ID_BUTTON_TX_RX,self.onButtonTx)

        wx.EVT_TOGGLEBUTTON(self,ID_BUTTON_PA,self.onButtonPA)

        wx.EVT_TOGGLEBUTTON(self,ID_BUTTON_TX_POWER_LEVEL,self.onButtonTxPowerLevel)

        wx.EVT_TOGGLEBUTTON(self,ID_BUTTON_MONITOR,self.onButtonMonitor)

        wx.EVT_TOGGLEBUTTON(self,ID_BUTTON_TX,self.onButtonTransmit)

        wx.EVT_TOGGLEBUTTON(self,ID_BUTTON_MUTE,self.onButtonMute)

        wx.EVT_TOGGLEBUTTON(self,ID_BUTTON_MUTE_SKYPE,self.onButtonMuteSkype)

        wx.EVT_TOGGLEBUTTON(self,ID_BUTTON_ON_OFF,self.onButtonOnOff)

        wx.EVT_TOGGLEBUTTON(self,ID_BUTTON_AUDIO_PA,self.onButtonAudioPA)

        wx.EVT_TOGGLEBUTTON(self,ID_BUTTON_EXT_ALARM,self.onButtonExtAlarm)

        # watch freq step here

        self.__do_layout()

        self.onButtonMonitor(None)

        self.m_sopen_last_time = {}

        self.m_sopen_last_time[0] = False

        self.m_sopen_last_time[1] = False

        init_power_state=True
        self.m_rig.setpower(init_power_state)
        self.m_on_off_button.SetValue(init_power_state)

        self.m_powered_on = self.m_rig.powered_on()

        if self.m_powered_on:
            self.m_freq=self.m_spin_ctrl_2.GetDefaultValue()*1E6
            self.init_rig()

        self.m_stay_muted = False

        self.m_squelch_refresh = 500

        self.m_cur_squelch_refresh = self.m_squelch_refresh

        self.m_tx_timer=TxTimer(self)

        return

    def onButtonTx(self,event):
        if self.m_tx_rx.GetValue():
            self.m_rig.enable_tx()

            # maybe get rid of this
            self.m_rig.disable_audio()
        else:
            self.m_rig.disable_tx()

            # maybe get rid of this
            self.m_rig.enable_audio()

        return

    def onButtonPA(self,event):
        if self.m_button_pa.GetValue():
            self.m_rig.enable_pa()
        else:
            self.m_rig.disable_pa()

        return

    def onButtonTxPowerLevel(self,event):
        if self.m_button_tx_power_level.GetValue():
            self.m_rig.set_tx_power_high()
        else:
            self.m_rig.set_tx_power_low()

        return

    def init_rig(self):
        self.m_rig.set_step(self.m_step)
        self.m_rig.set_rx_freq(self.m_freq)
        self.m_rig.set_tx_freq(self.m_freq)

        # start off in low power mode
        self.m_button_tx_power_level.SetValue(False)
        self.onButtonTxPowerLevel(None)

        # start off with ext alarm disabled
        self.m_ext_alarm_button.SetValue(False)
        self.onButtonExtAlarm(None)

        return

    def onButtonOnOff(self,event):
        if self.m_on_off_button.GetValue():
            self.m_rig.setpower(True)

        else:
            self.m_rig.setpower(False)

        self.m_powered_on = self.m_rig.powered_on()

        return

    def onButtonAudioPA(self,event):
        if self.m_audio_pa_button.GetValue():
            self.m_rig.enable_audio_pa()
        else:
            self.m_rig.disable_audio_pa()

        return

    def onButtonExtAlarm(self,event):
        if self.m_ext_alarm_button.GetValue():
            self.m_rig.enable_ext_alarm()
        else:
            self.m_rig.disable_ext_alarm()

        return

    def onButtonMonitor(self,event):
        if self.m_monitor_button.GetValue():
            unmute()

        return

    def onButtonTransmit(self,event):
        if self.m_tx_button.GetValue():
            self.m_tx_rx.SetValue(True)
            if len(sys.argv) > 1:
                # check frequency before enabling PA
                # maybe do not allow tx on 70.3875 or 70.4125
                self.m_button_pa.SetValue(True)
            mute()
            self.m_stay_muted=True
            self.m_tx_timer.Start(1000*60*5)
        else:
            time.sleep(0.3)
            self.m_tx_timer.Stop()
            unmute()
            self.m_stay_muted=False
            self.m_tx_rx.SetValue(False)
            self.m_button_pa.SetValue(False)

        self.onButtonTx(event)
        self.onButtonPA(event)

        return

    def onButtonMute(self,event):
        if self.m_mute_button.GetValue():
            mute()
            self.m_stay_muted=True
        else:
            unmute()
            self.m_stay_muted=False

        return

    def onButtonMuteSkype(self,event):
        skype_playback_jack_port="skype_playback_mixer"
        skype_left="MAIN L"
        skype_right="MAIN R"
        skype_left_connection='%s:"%s" system:playback_1'%(skype_playback_jack_port,skype_left)
        skype_right_connection='%s:"%s" system:playback_2'%(skype_playback_jack_port,skype_right)
        if self.m_mute_button_skype.GetValue():
            os.system("jack_disconnect %s" % skype_left_connection)
            os.system("jack_disconnect %s" % skype_right_connection)
        else:
            os.system("jack_connect %s" % skype_left_connection)
            os.system("jack_connect %s" % skype_right_connection)

        return

    def OnSquelchFloatSpin(self,event):
        floatspin = event.GetEventObject()

        newval = floatspin.GetValue()

#        print newval
#        print  self.m_squelch_level_value

        if newval > self.m_squelch_level_value:
            diff = newval - self.m_squelch_level_value

            for i in range(int(diff)):
                self.m_rig.move_squelch("up")
        else:

            diff = self.m_squelch_level_value - newval

            for i in range(int(diff)):
                self.m_rig.move_squelch("down")
        
        self.m_squelch_level_value = newval

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

        self.m_freq = nv

        self.m_rig.set_rx_freq(self.m_freq)
        self.m_rig.set_tx_freq(self.m_freq)

        return

    def __do_layout(self):
        sizer_1 = wx.BoxSizer(wx.HORIZONTAL)

        sizer_1.Add(self.m_spin_ctrl_1 , 0, wx.ADJUST_MINSIZE, 0)

        sizer_1.Add(self.m_mute_button, 0, wx.ADJUST_MINSIZE, 0)

        add_skype_mute=False
        for olen in (2,3):
            if len(sys.argv)==olen:
                if sys.argv[olen-1]=="-s":
                    add_skype_mute = True
        if add_skype_mute:
            sizer_1.Add(self.m_mute_button_skype, 0, wx.ADJUST_MINSIZE, 0)
        
        sizer_1.Add(self.m_tx_rx, 0, wx.ADJUST_MINSIZE, 0)
        sizer_1.Add(self.m_button_pa, 0, wx.ADJUST_MINSIZE, 0)
        sizer_1.Add(self.m_button_tx_power_level, 0, wx.ADJUST_MINSIZE, 0)
        sizer_1.Add(self.m_monitor_button, 0, wx.ADJUST_MINSIZE, 0)
        sizer_1.Add(self.m_tx_button, 0, wx.ADJUST_MINSIZE, 0)
        sizer_1.Add(self.m_on_off_button, 0, wx.ADJUST_MINSIZE, 0)
        sizer_1.Add(self.m_audio_pa_button, 0, wx.ADJUST_MINSIZE, 0)
        sizer_1.Add(self.m_ext_alarm_button, 0, wx.ADJUST_MINSIZE, 0)

        sizer_1.Add(self.m_spin_ctrl_2 , 0, wx.ADJUST_MINSIZE, 0)

        sizer_1.Add(self.m_led2, 0, wx.ADJUST_MINSIZE, 0)

        self.m_step_combo.SetStringSelection("6.25")

        self.Bind(wx.EVT_COMBOBOX, self.OnStepSelected, self.m_step_combo) 

        sizer_1.Add(self.m_step_combo, 0, wx.ADJUST_MINSIZE, 0)

        sizer_1.Add(self.m_spin_ctrl_squelch_level, 0, wx.ADJUST_MINSIZE, 0)

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
        os.system("lsmod | grep -q ftdi_sio && while ! rmmod ftdi_sio; do sleep 1; done")
        app = MyApp(clearSigInt=True)
        app.MainLoop()
        mute()        

    except KeyboardInterrupt:
        mute()
        sys.exit(1)

