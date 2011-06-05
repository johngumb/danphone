import sys
import os
import time

import ft232r
import DanPhone

#D0 - DATA
#D1 - CLK
#D2 - STB RX
#D3 - STB TX
#D4 - LD RX
#D5 - LD TX
#D6 - RX DATA
#D7 - TX en

if __name__=="__main__":

    os.system("lsmod | grep -q ftdi_sio && rmmod ftdi_sio")
    #os.system("/usr/bin/chrt -r -p 99 %s"%os.getpid())

    rig = DanPhone.DanPhone()

    rig.set_step(10E3)

    rig.init_rx()

    rig.enable_tx()

    rig.set_rx_freq(70.3875E6)
    #rig.set_rx_freq(75E6)

    rig.set_tx_freq(70.3875E6)
#    rig.set_tx_freq(70.2E6)

    print

    hwif = rig.get_hwif()

    hwif.bb.port|=0x40
    time.sleep(0.001)
    hwif.bb.port&=~0x40

    for i in range(100):
        #hwif.bb.ftdi_usb_purge_rx_buffer()
        hwif.bb.ftdi_fn.ftdi_usb_purge_rx_buffer()
        print hwif.bb.port & 0x30
        time.sleep(1)
