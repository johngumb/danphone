import ft232r
import time

f=ft232r.ft232r()

while True:
    f.setboolbit(0x80,1)
    time.sleep(0.5)
    f.setboolbit(0x80,0)
    time.sleep(0.5)
