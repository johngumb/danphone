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
    #offset=25000
    offset=11
#    d0,f0=refosc_lookup[offset]
    for [d,f] in refosc_lookup[offset:]:
        dlist.append(float(d))
        flist.append(float(f))

    p, pc = optimize.curve_fit(testfunc, dlist, flist)

    print(p)
    print(pc)
    print(testfunc(30495, *p))
    print(testfunc(59798, *p))
    plt.figure(figsize=(6, 4))
    #plt.scatter(dlist, flist, label='Data')
    r = [ testfunc(d,*p) for d in dlist ]
    #r = [ testfunc(d,0,p[1],p[2],0) for d in dlist ]
    #e = [ testfunc(d,p[0],0,0,0) for d in dlist ]
    plt.plot(dlist,r,'r--')
    plt.show()

    
# likely soln for 4m:
# [  8.94280480e-02  -1.35305578e+07   3.62469564e+03  -2.08975916e+03]

# from step16 run:
[  9.08434193e-02  -8.05129197e+06   1.00196697e+04  -2.25300667e+03]

#[  9.42309360e-02  -2.52287644e+01   1.00000000e+00  -2.61706606e+03]
def testfunc(x,a,b,c,d):
    #print(x,a,b,c,d)
    return (a * x) + (b/(x-c)) + d


if __name__=="__main__":
    fit()
