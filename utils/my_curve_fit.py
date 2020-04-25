import csv
import numpy
from scipy import optimize
import sys
import matplotlib.pyplot as plt
import array

def fit():
    with open(sys.argv[1]) as csvfile:
        reader = csv.reader(csvfile)
        refosc_lookup = [ row for row in reader ]

    dlist=[]
    flist=[]
    #offset=230
    offset=3743
    offset=820
    offset=140
    offseth=len(refosc_lookup)
    offseth=2214
    print(offseth)
#    d0,f0=refosc_lookup[offset]
    for [d,f] in refosc_lookup[offset:offseth]:
        dlist.append(float(d))
        flist.append(float(f))

    funcy = testfunc
    funcyh = testfunc
    p, pc = optimize.curve_fit(funcy, dlist, flist)

    print(p)
    print(pc)
    print(funcy(0xBF00, *p))
    print(funcy(59798, *p))
    plt.figure(figsize=(6, 4))
    #plt.scatter(dlist, flist, label='Data')
    r = [ funcyh(d,*p) for d in dlist ]
    #r = [ testfunc(d,0,p[1],p[2],0) for d in dlist ]
    #e = [ testfunc(d,p[0],0,0,0) for d in dlist ]
    dr = [ f - rv for (f,rv) in zip(flist,r) ]
    #plt.plot(dlist,flist,'r--')
    #plt.plot(dlist,r,'r--')
    plt.plot(dlist,dr,'r--')
    #plt.scatter(dlist,dr,label='Data')
    plt.show()

    
# likely soln for 4m:
# [  8.94280480e-02  -1.35305578e+07   3.62469564e+03  -2.08975916e+03]

# from step16 run:
#[  9.08434193e-02  -8.05129197e+06   1.00196697e+04  -2.25300667e+03]

#[  9.42309360e-02  -2.52287644e+01   1.00000000e+00  -2.61706606e+03]
def testfunc(x,a,b,c,d):

    return (a * x) + (b/(x-c)) + d

def testfuncs(x,a,b,c,d,e,f):

    return (a * x) + (b/(x-c)) + d + (e/((x**3)-f))


def testfuncl(x,a,b):
    #print(x,a,b,c,d)
    return (a * x) + b

def testfunclh(x,a,b):
    #print(x,a,b,c,d)
    
    if x<40000:
        a-=0.0001
        b+=1
    return (a * x) + b

def testfunch(x,a,b,c,d):
    #print(x,a,b,c,d)
    
    if x>30000:
        a+=0.00002
        b+=10

    return (a * x) + (b/(x-c)) + d

import math
def inv_func(freq):
    #
    # from dacdata dacdata-2m-step1-opamp-linear.csv by curve fitting
    # using (a * x) + (b/(x-c)) + d
    #
    [a, b, c, d] = [1.71348244e-01, -1.43946879e+06, 2.49851466e+04, -6.28558918e+03]



    P=[a,b,c,d]
#    x=5000
#    y = testfunc(x,*P)
#    print(y)

    y=F
    A=a
    B=d - y - a * c
    C=b-(d*c)+(y*c)

    #test to prove we have correct quadratic parameters
    #print(A*(x**2) + B*x + C)

    print (A,B,C)
    print(math.sqrt(B**2 - 4*A*C))
    r = (-B + math.sqrt(B**2-(4*A*C)))/(2*A)

    return r
if __name__=="__main__":
    F=2000
    print(hex(int(inv_func(F))))
    fit()
