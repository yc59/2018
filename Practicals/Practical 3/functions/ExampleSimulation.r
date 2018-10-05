#Example simultion code
#Author: Len Thomas
#Last updated: 25th October 2013

#Functions to run a simulation to look at the MSE arising from 
# using trimmed means to estimate the mean of a mixture distribution
# See ExampleSimulation.pdf for details
#Some code to drive the simulation is provided in ExampleSimulationDriver.r

#Note that there are two versions of the functions mix.dist and RMSE
#Have a look at them, and try to decide which is better!...

do.sim <- function(mix.par=0.2,mu1=5,sigma1=4,mu2=5,df=4,n=30,
                   alpha.vec=c(0,0.1,0.2), M=1000) {
#Purpose:
# Runs a simulation to compute the RMSEs for trimmed means from 
#  samples taken from a 2 point normal and t mixture model
#Inputs:
# mix.par - mixture parameter of the distribution (range 0-1)
# mu1 - mean of the normal component of the mixture
# sigma1 - sd of the normal component of the mixture
# mu2 - mean of the t component of the mixture
# df - degrees of freedom for the t component
# n - size of samples to generate in simulation
# alpha.vec - vector of trimming levels to try (0=no trim i.e. mean, 
#   0.5=trim 50th quantile above and below - i.e., median)
# M - number of simultions to run
#Output: List containing elements:
# MSE - vector of the MSEs at each trim level
# ests - matrix with M rows and length(alpha.vec) columns containing the raw estimates
# mu - the true mean of the input distribution
  

  #initialize matrix of outputs
  est.matrix <- matrix(NA,M,length(alpha.vec)) 
  #run simulation
  for(simulation in 1:M) {
    temp <- mix.dist(n,mix.par,mu1,sigma1,mu2,df) 
    est.matrix[simulation,] <- estimate(temp,alpha.vec) 
  }

  #work out the expected value of the input distribution
  E.Z<-get.E.Z(mix.par,mu1,mu2)
  
  #Calculate MSEs   
  mean.square.error <- MSE(est.matrix,E.Z)

  return(list(MSE=mean.square.error, ests=est.matrix, mu=E.Z))

} 

get.E.Z <- function(mix.par, mu1, mu2){
#Purpose: Returns the expected value of a mixture with mix proportion
# mix.par, and means mu1 and mu2
  return(mix.par*mu1+(1-mix.par)*mu2)
}


mix.dist <- function(n,mix.par,mu1,sigma1,mu2,df) {
#Purpose: Returns n samples from the mixture distribution
#Inputs:
# n - number of samples to return
# mix.par - pi in the practical notes -- proportion of samples from norm dist
# mu1 - mean of norm dist
# sigma1 - sd of norm dist
# mu2 - mean of t dist
# df - df for t dist
#Outputs:
# vector of n values from the mixture distribution
#Implementation note:
# This version uses vectorization - but wastefully samples from
# both the normal and t for each observation.  See below for another
# way to do this. Which is quicker, I wonder?
 
  #sample n values from a Bernoulli dist, to see which distribution
  # each data point comes from
  nu <- rbinom(n,1,mix.par)
  
  #sample values from the normal
  X <- rnorm(n,mu1,sigma1)
  #sample values from the t
  Y <- mu2 + rt(n,df)
  
  #put them together
  Z <- nu*X + (1-nu)*Y
  return(Z)
}

mix.dist.alt <- function(n,mix.par,mu1,sigma1,mu2,df) {
#Purpose: Alternative implementation of routine to sample n values
#  from the mixture distribution
#Inputs:
# n - number of samples to return
# mix.par - pi in the practical notes -- proportion of samples from norm dist
# mu1 - mean of norm dist
# sigma1 - sd of norm dist
# mu2 - mean of t dist
# df - df for t dist
#Outputs:
# vector of n values from the mixture distribution

  #create results vector
  Z<-numeric(n)
  
  #go through each observation...
  for(i in 1:n){
    #...determine if it comes from the normal or t distribution
    if(rbinom(1,1,mix.par)==1) {
      #normal distribution
      Z[i]<-rnorm(1,mu1,sigma1)
    } else {
      #t distribution
      Z[i]<-mu2+rt(1,df)
    }
  }

  return(Z)
}


estimate <- function(x,trim) {
#Purpose: Estimates trimmed means on data x, using a vector of trimming
# levels trim
#Inputs:
# x - data
# trim - vector of trimming levels
#Outputs:
# vector of trimmed means

  #create vector for results
  n.trimmed.means<-length(trim)
  trimmed.means<-numeric(n.trimmed.means)

  #loop through, calculating trimmed means
  for(i in 1:n.trimmed.means) 
    trimmed.means[i] <- mean(x,trim[i])

  return(trimmed.means)
}

MSE <- function(theta.hat,theta) {
#Purpose: Calculates mean squared errors for a matrix of estimates
# where the rows are the replicates and the columns are different
# estimators
#Inputs:
# theta.hat - vector of estimates.  Rows = replicates; Cols = estimators.
# theta - true value
#Outputs:
# vector of MSEs - one for each estimator
#Implementation note:
# This version uses a double loop.  See below for a more vectorized 
#  version of this function.  Which is quicker?

  #count numeber of replicates for each estimator
  n <- dim(theta.hat)[1]
  #count number of estimators
  num.ests <- dim(theta.hat)[2]
  #create vector for results
  MSE.vec <- numeric(num.ests)

  #Work out the sum of squared errors for each estimator:
  #Go through each replicate
  for(i in 1:n) {
    #go through each estimator
    for(j in 1:num.ests) 
      #Add the sqared error for this replicate of the estimator to
      # the running total in MSE.vec
      MSE.vec[j] <- MSE.vec[j]+(theta.hat[i,j]-theta)^2
  }
  #Turn from a sum of squared errors into mean square error by dividing
  # by n
  MSE.vec <- MSE.vec/n

  return(MSE.vec)
} 

MSE.alt <- function(theta.hat,theta) {
#Purpose: Alternative vectorized implementation of function to
# calculate mean squared errors for a matrix of estimates
# where the rows are the replicates and the columns are different
# estimators
#Inputs:
# theta.hat - vector of estimates.  Rows = replicates; Cols = estimators.
# theta - true value
#Outputs:
# vector of MSEs - one for each estimator

  #Declare function to calculate MSE of a vector
  MSE.vec <- function(theta.hat.vec, theta) {
  #Purpose: calculates MSE of a vector
  #Inputs: 
  # theta.hat.vec - vector of estimates
  # theta - true value
  #Output: MSE
     square.error<-(theta.hat.vec-theta)^2
     MSE<-mean(square.error)
     return(MSE)
  }

  #Apply this function to each column of theta.hat
  MSE<-apply(theta.hat,2,MSE.vec,theta)

  return(MSE)
} 
