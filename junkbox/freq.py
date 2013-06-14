import sys

if __name__=="__main__":
    freq=float(sys.argv[1])*1E6

    m_refclk=14.4E6
    m_prescale_divide=40
    m_r=14.4E6/12.5E3
    #m_r=14.4E6/10.0E3

    if len(sys.argv)>2:
    	freq=freq+21.4E6

    x = (freq * m_r)/m_refclk

    n = int(x/m_prescale_divide)

    a = int(round(x-n*m_prescale_divide))

    encoded = ((n<<7) + a)*2

    print "%x" % encoded

    print "%d %d" % (((encoded & 0xFFFF) >> 16),(encoded & 0xFFFF))
