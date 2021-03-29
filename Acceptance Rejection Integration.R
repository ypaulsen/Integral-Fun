# Acceptance/Rejection Integration 

# EG Normal distribution   

# The following program uses Acceptance/Rejection integration to calculate the 
# area under the curve of the standard normal distribution over the interval 
# (-1.96, 1.96). This integral is well known and while there is no exact closed 
# form solution over any given range, statisticians will recognize that this 
# interval contains 95% of a probability density function and so the exact 
# solution is known as 0.95.   

# This program performs Acceptance/Rejection integration on the function defined
# as f() over the interval (a, b). For demonstration purposes, this example uses
# the normal distribution over a widely studied interval since it has a well 
# known and predictable outcome.  


# Function  
f <- function(x) dnorm(x)             # Input function to analyze

# Interval     
a <- -1.959964; b <- 1.959964         # Interval containing 95% of a pdf

# Maximum value of f(x) over (a,b)  
max <- optimize(f, c(a,b), maximum = T)$objective

n <- 1000000                          # Number of random points to evaluate
x <- runif(n, a, b)                   # n Random x values from U(a,b)
y <- runif(n, 0, max)                 # n Random y values from U(0, max)
p <- mean(y < f(x))                     # Proportion of y values less than f(x)
A <- (b-a)*max                        # Area of entire region  
p*A                                   # Proportion of area under the curve     


# The output of this program varies by the results of the random number 
# generator. To evaluate the performance of this integration I'll look at the 
# mean absolute error (MAE) of the output. The integral here has a known 
# solution of 0.95. This for-loop performs the same integration 100 times checks 
# the results against the known solution and outputs MAE.  


set.seed(pi)

absolute_error <- c()

for(i in 1:100){
  x <- runif(n, a, b)
  y <- runif(n, 0, max)
  p <- mean(y < f(x))
  A <- (b-a)*max
  I <- p*A
  absolute_error[i] <- abs(0.95 - I)
}

mean(absolute_error)


# With the supplied seed, the mean absolute error is just 0.00062. Or around 
# 0.065%.  