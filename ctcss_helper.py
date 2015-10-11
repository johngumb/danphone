def get_ctcss(tx_freq):
        if tx_freq in [51.34E6, 51.35E6, 51.3E6, 50.52E6]: # GB3AM, GB3CT, GB3ZY, GB3WX
            result = 77.0
        elif tx_freq in [51.32E6]: # GB3XD
            result = 71.9
        elif tx_freq in [51.31E6]: # GB3FX
            result = 82.5
        elif tx_freq==51.27E6: # GB3DB
            result = 110.9
        elif tx_freq in [145.025E6]: # MH
            result = 88.5
        elif tx_freq in [145.075E6, 145.1625E6, 145.050E6, 145.1E6]: # RD, NE, WH, VA
            result = 118.8
#            result = 110.9
        elif tx_freq in [145.0875E6]: # EA
            result = 110.9
            #result = 71.9
            #result = 82.5
        elif tx_freq in [145.1125E6]: # KY, LA
            result =94.8
#            result = 103.5
        elif tx_freq in [145.125E6]: # SN
            result = 71.9
#            result = 110.9           #DA
        elif tx_freq in [145.1375E6]: # AL
            result = 77.0
        elif tx_freq in [145.1875E6]: # JB, BF
#            result = 77.0
            result = 103.5
        elif tx_freq in [145.0E6]: # 
#            result = 88.5
#            result = 94.8 # WR
            result = 77.0 # CF
        elif tx_freq in [145.150E6]: # GB3WS
            result = 88.5
        elif tx_freq in [145.175E6]: # GB3FR
            result = 71.9
        else:
            result = 0

        return result
