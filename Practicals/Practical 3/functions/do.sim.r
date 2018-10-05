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
    est.matrix[simulation, ] <- estimate.mean(temp,alpha.vec) 
  }
  
#work out the expected value of the input distribution
  E.Z <- get.expectation(mix.par,mu1,mu2)
  
#Calculate MSEs   
  mean.square.error <- mean.square.error(est.matrix,E.Z)
  
  return(list(MSE=mean.square.error, ests=est.matrix, mu=E.Z))
  
} 