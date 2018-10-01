#Example randomization code, written (mostly) in class
#30/10/2013

martin<-c(0.13, -0.01, -0.01, 0.42, -0.02, 0.01, 0.09, 0.03, 0.04, 0.06, 0.12, 0.03)

rand.func<-function(data,n=999,H0=0){
#Purpose: Perform randomziation test on a mean of a univariate data sample
#Inputs:
# data - vector of data
# n - number of resamples
# H0 - null hypothesis mean value
#Outputs:
# p-value of test

  #Error checking code should go here
  
  #vector to store the sim means means
  resample.means<-numeric(n+1)
  #create symetric dataset and turn to all positive
  centered.pos.data<-abs(data-H0)
  n.data<-length(data)
  
  #Repeat the following two steps many times:
  for(i in 1:n){
    #(a) Simulate a data set according to H0
    #  simulate random signs
    signs<-sample(c(-1,1),n.data,replace=T)
    sim.data<-centered.pos.data*signs
    #(b) Calculate T(x) using the simulated data
    resample.means[i]<-mean(sim.data)
  }

  #Add T(x) evaluated from the sample data
  resample.means[n+1]<-mean(data)-H0

  #p-value is the proportion of the T(x)s as extreme or more 
  # extreme than the one from the sample data
  p.value<-sum(abs(resample.means)>=abs(resample.means[n+1]))/(n+1)
  
  return(p.value)
}

x<-rand.func(martin,n=10000)
print(x)
