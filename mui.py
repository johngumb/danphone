#!/usr/bin/python
#
# OpenPMR - tools to make old PMR radios useful.
#
# Copyright (C) 2013,2014  John Gumb, G4RDC
#
# This file is part of OpenPMR.

# OpenPMR is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the icense, or
# (at your option) any later version.

# OpenPMR is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with OpenPMR.  If not, see <http://www.gnu.org/licenses/>.

# TODO

# add persistence of settings

# spinctrl.py


import sys
import os
import time
import tempfile

import wx
import ledthing
#from agw import floatspin
import wx.lib.agw.floatspin as FS

import McMicro


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
ID_BUTTON_AUDIO_DISABLE=wx.NewId()
ID_BUTTON_TX_SAFETY=wx.NewId()

MUTED = False

g_audioserver=""
g_rig = None

def sixmetres():
    return len(sys.argv) > 1 and sys.argv[1]=="-6"

def sdrmute():
    return
    os.system("/home/john/sdr off")

def sdrunmute():
    return
    os.system("/home/john/sdr on")

# TODO fix initial mute state
# TODO radio might start with signal present.
def mute(audioserver):
    global MUTED
    if not MUTED:
        os.system("jack_disconnect %s:from_slave_2 system:playback_1" % audioserver)
        os.system("jack_disconnect %s:from_slave_2 system:playback_2" % audioserver)
        MUTED = True
    return

def unmute(audioserver):
    global MUTED
    if MUTED:
        os.system("jack_connect %s:from_slave_2 system:playback_1" % audioserver)
        os.system("jack_connect %s:from_slave_2 system:playback_2" % audioserver)
        MUTED = False
    return

class ScanTimer(wx.Timer):
    def __init__(self,target):
        wx.Timer.__init__(self)
        self.target = target
        self.m_idx=0
        return

    def Notify(self):
        wx.WakeUpIdle()

        freqs = (70.45, 70.2)
        
        if os.path.exists("/tmp/scan") and not self.target.m_rig.squelch_open():
            self.m_idx +=1
            if self.m_idx==len(freqs):
                self.m_idx=0
            freq = freqs[self.m_idx]

            self.target.m_spin_ctrl_2.SetValue(freq)

            freqm = freq*1.0E6
            self.target.m_rig.set_rx_freq(freqm)
            self.target.m_rig.set_tx_freq(freqm)

            self.Start(3000)

        return

class TxTimer(wx.Timer):
    def __init__(self,target):
        wx.Timer.__init__(self)
        self.target = target
        self.m_tx_timeout_count = 0
        self.m_tx_timeout_threshold = 12
        return
    
    def Notify(self):

        if self.target.m_tx_button.GetValue():
            #
            # longer timeout in QRP mode
            #
            print "tx timeout",
            if self.m_button_tx_power_level.GetValue():
                print
                self.m_tx_timeout_count = self.m_tx_timeout_threshold
            else:
                print ", low power"
                self.m_tx_timeout_count +=1

            if self.m_tx_timeout_count == self.m_tx_timeout_threshold:
                self.target.m_tx_button.SetValue(False)
                self.target.onButtonTransmit(None)

        wx.WakeUpIdle()

        return

class TxSafetyTimer(wx.Timer):
    def __init__(self,target):
        wx.Timer.__init__(self)
        self.target = target
        return
    
    def Notify(self):

        if self.target.m_tx_safety_button.GetValue():
            self.target.m_tx_safety_button.SetValue(False)

        self.Stop()

        wx.WakeUpIdle()

        return

class StatusLEDtimer(wx.Timer):
    def __init__(self,target,dur=500):
        wx.Timer.__init__(self)
        self.target = target

        self.m_counts_initialised=False

        self.Start(dur)

        return

    def init_counts_if_not_done(self):

        if self.m_counts_initialised:
            return

        if self.target.m_devid=="cli":

            self.m_squelch_sample = False

            #
            # consider init counts function
            #
            self.m_lock_max_count = 1

            self.m_power_max_count = 1

            self.m_lock_count = self.m_lock_max_count

            self.m_power_count = self.m_power_max_count
        else:
            self.m_squelch_sample = True

            self.m_lock_max_count = 4

            self.m_power_max_count = 8

            self.m_lock_count = self.m_lock_max_count

            self.m_power_count = self.m_power_max_count
            
        self.m_counts_initialised = True

    def Notify(self):
        """Called every timer interval"""

        #
        # HACK
        # during TX stop clicking
        # during this time the LEDs will not update
        # power state will not be updated 
        #
        if self.target.m_tx and not self.target.m_devid=="cli":

            #
            # HACK not sure what wx.WakeUpIdle does
            # Where there are returns elsewhere in this file
            # they should probably have this.
            #
            wx.WakeUpIdle()
            return

        self.init_counts_if_not_done()

        if self.m_power_count == self.m_power_max_count:
            self.target.check_for_power_event()

            self.m_power_count = 0

        self.m_power_count = self.m_power_count + 1

        if not self.target.m_powered_on:
            self.target.m_led2.SetState(3)
            self.target.m_squelch_led.SetState(3)
            return

        if self.m_lock_count == self.m_lock_max_count:
            if self.target.m_rig.locked():
                self.target.m_led2.SetState(2)
            else:
                self.target.m_led2.SetState(0)

            self.m_lock_count = 0

        self.m_lock_count = self.m_lock_count + 1

        sopen=self.target.m_rig.squelch_open()

        if not self.m_squelch_sample:
            if sopen:
                self.target.m_squelch_led.SetState(2)
                if not self.target.m_monitor_button.GetValue():
                    if not self.target.m_stay_muted:
                        unmute(self.target.m_audioserver)

                    self.target.m_rig.enable_audio_pa()
            else:
                self.target.m_squelch_led.SetState(0)

                if not self.target.m_monitor_button.GetValue():
                    mute(self.target.m_audioserver)

                if not self.target.m_audio_pa_button.GetValue():
                    self.target.m_rig.disable_audio_pa()
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
                        unmute(self.target.m_audioserver)
                        self.target.m_rig.enable_audio_pa()

            if (not sopen) and lastclosed:
                self.target.m_squelch_led.SetState(0)
                if not self.target.m_monitor_button.GetValue():
                    mute(self.target.m_audioserver)
                if not self.target.m_audio_pa_button.GetValue():
                    self.target.m_rig.disable_audio_pa()

#        self.target.m_sopen_last_time[3] = self.target.m_sopen_last_time[2]

#        self.target.m_sopen_last_time[2] = self.target.m_sopen_last_time[1]

#        self.target.m_sopen_last_time[1] = self.target.m_sopen_last_time[0]

#        self.target.m_sopen_last_time[0] = sopen

            for i in range(samples_to_check-1, 0, -1):
                if self.target.m_sopen_last_time.has_key(i-1):
                    self.target.m_sopen_last_time[i] = self.target.m_sopen_last_time[i-1]
            self.target.m_sopen_last_time[0] = sopen

        wx.WakeUpIdle()

        return

class MyFrame(wx.Frame):
    def __init__(self, *args, **kwds):
        global g_audioserver
        global g_rig

        # begin wxGlade: MyFrame.__init__
        kwds["style"] = wx.DEFAULT_FRAME_STYLE
        wx.Frame.__init__(self, *args, **kwds)

        self.m_rig = McMicro.McMicro()

        g_rig = self.m_rig

        self.m_min_freq=35.9E6

        self.m_max_freq=450E6

        # linear attatched to Aux button
        #self.m_aux_linear=True

        if sixmetres():
            self.m_devid="cli"
            self.m_audioserver="skate"
        else:
            self.m_devid="MCVEC40K"
            self.m_audioserver="dab"

        g_audioserver=self.m_audioserver
        self.m_rig.initialise(device_id=self.m_devid)

        self.m_spin_ctrl_1 = FS.FloatSpin(self, ID_SPIN_1)
        
        self.m_spin_ctrl_2 = FS.FloatSpin(self, ID_SPIN_2)

        self.m_led2=ledthing.LED(self,ID_LED_2)

        self.m_squelch_led=ledthing.LED(self,ID_LED_SQUELCH)

        steps=["Auto","4","5","6.25","8","10","12.5"]

        if sixmetres():
            self.m_step_selected = "10"
        else:
            self.m_step_selected = "12.5"

        self.m_step = float( self.m_step_selected ) * 1000

        self.m_rig.set_step(self.m_step)

        self.m_spin_ctrl_squelch_level = FS.FloatSpin(self, ID_SPIN_2)

        self.m_step_combo = wx.ComboBox(self, -1, self.m_step_selected, choices=steps)

        self.m_digits = 5
#        print dir(self.m_spin_ctrl_2 )
        for f in [self.m_spin_ctrl_1, self.m_spin_ctrl_2]:
            f.SetFormat("%F")
            f.SetDigits(self.m_digits)
            if sixmetres():
                self.m_freq=50.840E6
            else:
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

        self.m_disable_audio_button = wx.ToggleButton(self, ID_BUTTON_AUDIO_DISABLE, "AUDIO")

        self.m_tx_safety_button = wx.ToggleButton(self, ID_BUTTON_TX_SAFETY, "TxSafety")

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

        wx.EVT_TOGGLEBUTTON(self,ID_BUTTON_AUDIO_DISABLE,self.onButtonAudioDisable)
        wx.EVT_TOGGLEBUTTON(self,ID_BUTTON_TX_SAFETY,self.onButtonTxSafety)

        # watch freq step here

        self.__do_layout()

        self.onButtonMonitor(None)

        self.m_sopen_last_time = {}

        self.m_sopen_last_time[0] = False

        self.m_sopen_last_time[1] = False

        self.m_powered_on = self.m_rig.powered_on()

        self.m_last_powered_on = False

        init_power_state=True
#        self.m_rig.setpower(init_power_state)
        self.m_on_off_button.SetValue(init_power_state)

        self.onButtonOnOff(None)

        self.m_freq=self.m_spin_ctrl_2.GetDefaultValue()*1E6

        self.m_stay_muted = False

        self.m_squelch_refresh = 500

        self.m_cur_squelch_refresh = self.m_squelch_refresh

        self.m_tx_timer=TxTimer(self)

        self.m_tx_safety_timer=TxSafetyTimer(self)

        self.m_tx = False

        self.m_txlockdir="/var/run/mui/txlock"

        if not os.path.exists(self.m_txlockdir):
            os.makedirs(self.m_txlockdir)

        if not sixmetres():
            self.m_scan_timer=ScanTimer(self)
            self.m_scan_timer.Start(3000)
        
        self.m_tx_lockfile = None

        # lock stays off for 8 minutes
        self.m_tx_safety_timeout=8*60*1000

        return

    def get_tx_lock(self):
        self.m_tx_lockfile = None

        if len(os.listdir(self.m_txlockdir)) > 0:
            return False

        (_, self.m_tx_lockfile) = tempfile.mkstemp(dir=self.m_txlockdir)

        return True

    def free_tx_lock(self):

        if self.m_tx_lockfile and os.path.exists(self.m_tx_lockfile):
            os.unlink(self.m_tx_lockfile)

        self.m_tx_lockfile = None

        return

    def onButtonTx(self,event):
        if self.m_tx_rx.GetValue():
            self.m_rig.enable_tx()

            self.m_tx = True

            # maybe get rid of this
            self.m_rig.disable_audio()
        else:
            self.m_rig.disable_tx()

            self.m_tx = False

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
            #if self.m_aux_linear:
            #    self.m_ext_alarm_button.SetValue(False)
            #    self.onButtonExtAlarm(event)  
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

    def check_for_power_event(self):
        self.m_powered_on = self.m_rig.powered_on()

        if self.m_powered_on and not self.m_last_powered_on:
            print "rig just powered on"
            self.init_rig()

        if not self.m_powered_on and self.m_last_powered_on:
            print "rig just powered off"

        self.m_last_powered_on = self.m_powered_on

        return

    def onButtonOnOff(self,event):
        if self.m_on_off_button.GetValue():
            self.m_rig.setpower(True)
        else:
            self.m_rig.setpower(False)

        self.check_for_power_event()

        return

    def onButtonAudioPA(self,event):
        if self.m_audio_pa_button.GetValue():
            self.m_rig.enable_audio_pa()
        else:
            self.m_rig.disable_audio_pa()

        return

    def onButtonExtAlarm(self,event):
        if self.m_ext_alarm_button.GetValue():
            #if self.m_aux_linear:
            #    self.m_button_tx_power_level.SetValue(False)
            #    self.onButtonTxPowerLevel(event)
            self.m_rig.enable_ext_alarm()
        else:
            self.m_rig.disable_ext_alarm()

        return

    def onButtonAudioDisable(self,event):
        if self.m_disable_audio_button.GetValue():
            self.m_rig.disable_audio()
        else:
            self.m_rig.enable_audio()

        return

    def onButtonMonitor(self,event):
        if self.m_monitor_button.GetValue():
            unmute(self.m_audioserver)

        return

    def onButtonTxSafety(self,event):
        if self.m_tx_safety_button.GetValue():
            self.m_tx_safety_timer.Start(self.m_tx_safety_timeout)
        else:
            self.m_tx_safety_timer.Stop()
            self.m_tx_button.SetValue(False)
            self.onButtonTransmitAction(event)

    def onButtonTransmitAction(self,event):
        if self.m_tx_button.GetValue() and self.get_tx_lock():
            self.m_tx_rx.SetValue(True)
            if len(sys.argv) > 1:
                # check frequency before enabling PA
                # maybe do not allow tx on 70.3875 or 70.4125
                if sys.argv[-1]=="p":
                    self.m_button_pa.SetValue(True)
            mute(self.m_audioserver)
            sdrmute()
            self.m_stay_muted=True
            self.m_tx_timer.Start(1000*60*7)
        else:
            self.free_tx_lock()
            time.sleep(0.3)
            self.m_tx_timer.Stop()
            if not self.m_monitor_button.GetValue():
                self.m_stay_muted=False
                unmute(self.m_audioserver)
            sdrunmute()
            self.m_tx_rx.SetValue(False)
            self.m_button_pa.SetValue(False)

        self.onButtonTx(event)
        self.onButtonPA(event)

        return

    def onButtonTransmit(self,event):

        if not self.m_tx_safety_button.GetValue():
            print "Tx safety catch on"
            self.m_tx_button.SetValue(False)
            return

        self.m_tx_safety_timer.Stop()
        self.m_tx_safety_timer.Start(self.m_tx_safety_timeout)

        self.onButtonTransmitAction(event)

        return

    def onButtonMute(self,event):
        if self.m_mute_button.GetValue():
            mute(self.m_audioserver)
            self.m_stay_muted=True

            # for now allow rig speaker to continue monitoring
            #self.m_rig.disable_audio_pa()
        else:
            unmute(self.m_audioserver)
            self.m_stay_muted=False

            # for now allow rig speaker to continue monitoring
            #self.m_rig.enable_audio_pa()

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

        self.m_freq=floatspin.GetValue()*1E6

        self.m_rig.set_rx_freq(self.m_freq)
        self.m_rig.set_tx_freq(self.m_freq)

        return

    def OnFStextEnter(self,event):
        floatspin = event.GetEventObject()

        self.OnFloatSpin(event)

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

        self.m_spin_ctrl_1._increment=self.m_step/1E6
        self.m_spin_ctrl_2._increment=self.m_step/1E6

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
        sizer_1.Add(self.m_on_off_button, 0, wx.ADJUST_MINSIZE, 0)
        sizer_1.Add(self.m_tx_button, 0, wx.ADJUST_MINSIZE, 0)
        sizer_1.Add(self.m_audio_pa_button, 0, wx.ADJUST_MINSIZE, 0)
        sizer_1.Add(self.m_ext_alarm_button, 0, wx.ADJUST_MINSIZE, 0)
        sizer_1.Add(self.m_disable_audio_button, 0, wx.ADJUST_MINSIZE, 0)
        sizer_1.Add(self.m_tx_safety_button, 0, wx.ADJUST_MINSIZE, 0)

        sizer_1.Add(self.m_spin_ctrl_2 , 0, wx.ADJUST_MINSIZE, 0)

        sizer_1.Add(self.m_led2, 0, wx.ADJUST_MINSIZE, 0)

        self.m_step_combo.SetStringSelection(self.m_step_selected)

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

        frame = MyFrame(None, -1, "MCmicro")
        frame.Show(True)
        self.SetTopWindow(frame)
        return True

if __name__=="__main__":
    try:
        os.system("lsmod | grep -q ftdi_sio && while ! rmmod ftdi_sio; do sleep 1; done")
        app = MyApp(clearSigInt=True)
        app.MainLoop()
        g_rig.m_request_thread_exit=True
        mute(g_audioserver)        

    except KeyboardInterrupt:
        g_rig.m_request_thread_exit=True
        mute(g_audioserver)

        sys.exit(1)

