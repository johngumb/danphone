# Python Program to find correlation coefficient. 
import math 
import csv

# function that returns correlation coefficient. 
def correlationCoefficient(X, Y, n) : 
	sum_X = 0
	sum_Y = 0
	sum_XY = 0
	squareSum_X = 0
	squareSum_Y = 0
	
	
	i = 0
	while i < n : 
		# sum of elements of array X. 
		sum_X = sum_X + X[i] 
		
		# sum of elements of array Y. 
		sum_Y = sum_Y + Y[i] 
		
		# sum of X[i] * Y[i]. 
		sum_XY = sum_XY + X[i] * Y[i] 
		
		# sum of square of array elements. 
		squareSum_X = squareSum_X + X[i] * X[i] 
		squareSum_Y = squareSum_Y + Y[i] * Y[i] 
		
		i = i + 1
	
	# use formula for calculating correlation 
	# coefficient. 
	corr = (float)(n * sum_XY - sum_X * sum_Y)/ \
		(float)(math.sqrt((n * squareSum_X -\
		sum_X * sum_X)* (n * squareSum_Y -\
		sum_Y * sum_Y)))
	return corr

def read_csv(datafile):
        with open(datafile) as csvfile:
            reader = csv.reader(csvfile)
            refosc_lookup = [ float(f) for (d,f) in reader ]

        return refosc_lookup

A=read_csv("dacdata.csv")
B=read_csv("dacdata-2m-step1-to1200.csv")
C=B[:len(A)]

# Driver function 
#X = [15, 18, 21, 24, 27] 
#Y = [25, 25, 27, 31, 32] 

X=A
Y=C

# Find the size of array. 
n = len(X) 

# Function call to correlationCoefficient. 
print ('{0:.6f}'.format(correlationCoefficient(X, Y, n))) 

# This code is contributed by Nikita Tiwari. 

