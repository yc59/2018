estimate.mean <- function(x,trim) {
#Purpose:
#  Estimates trimmed means on data x, using a vector of trimming levels
#Inputs:
# x - data
# trim - vector of trimming levels
#Outputs:
# vector of trimmed means

#create vector for results
  n.trimmed.means <- length(trim)
  trimmed.means <- numeric(n.trimmed.means)
  
#loop through, calculating trimmed means
  for(i in 1:n.trimmed.means) 
    trimmed.means[i] <- mean(x,trim[i])
  
  return(trimmed.means)
}