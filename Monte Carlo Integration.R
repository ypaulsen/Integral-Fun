# Monte Carlo Integration 

# EG Normal distribution   

# The following program uses Monte Carlo integration to calculate the area under
# the curve of the standard normal distribution over the interval (-1.96, 1.96).
# This integral is well known and while there is no exact closed form solution 
# over any given range, statisticians will recognize that this interval contains
# 95% of a probability density function and so the exact solution is known as 
# 0.95.   

# This program performs Monte Carlo integration on the function defined as f() 
# over the interval (a, b). For demonstration purposes, this example uses the 
# normal distribution over a widely studied interval since it has a well 
# known and predictable outcome.   


# Function  
f <- function(x) dnorm(x)             # Input function to analyze

# Interval     
a <- -1.959964; b <- 1.959964         # Interval containing 95% of a pdf

# Integration by Monte Carlo 
MC <- 10000000  
x <- runif(MC, a, b)
(b-a)*mean(f(x))

# The output of this program varies by the results of the random number 
# generator. To evaluate the performance of this integration I'll look at the 
# mean absolute error (MAE) of the output. The integral here has a known 
# solution of 0.95. This for-loop performs the same integration 100 times and 
# outputs MAE. 


set.seed(pi)

absolute_error <- c()

for(i in 1:100){
  x <- runif(MC, a, b)
  I <- (b-a)*mean(f(x))
  absolute_error[i] <- abs(0.95 - I)
}

mean(absolute_error)


# With the supplied seed, the mean absolute error is just 9.275e-05. Or around 
# 0.0098%.    
