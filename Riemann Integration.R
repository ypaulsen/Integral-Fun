# Riemann Integration 

# EG Normal distribution   

# The following program uses Riemann integration to calculate the area under the
# curve of the standard normal distribution over the interval (-1.96, 1.96). 
# This integral is well known. While there is no exact closed form solution for 
# this integral over an arbitrary range, the practiced statistician will 
# recognize that this interval contains 95% of a probability density function 
# and therefore the exact solution is known as 0.95.   


# This program performs a Riemann integration on the function defined as f() 
# over the interval (a, b). I have used the normal distribution in this example 
# over a well studied interval for demonstration purposes since it has a widely 
# known and predictable outcome.  


# Function  
f <- function(x) dnorm(x)                     # Input function to analyze

# Interval     
a <- -1.959964; b <- 1.959964                 # Interval containing 95% of a pdf

# Reimann Integration 
n <- 100000                                   # Number of intervals to partition
w <- (b-a)/n                                  # Width of each interval 
x <- seq(a + .5*w, b - .5*w, length.out = n)  # Vector of midpoints       
A <- w*f(x)                                   # Width by height   
sum(A)                                        # Riemann Sum   

# The output of this program is exactly equal to the known area under the normal
# curve over the provided interval: 0.95. 