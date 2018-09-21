#MT4113 Practical 1 Additional Practice solutions

#---------------------------------------------------------------------------------
#Part 1

#Question 1
x <- runif(1)
if(x >= 0.5) {
  cat("lucky\n")
} else {
  cat("unlucky\n")
}

#Question 2
x <- seq(-10, 10, by = 0.01)
y <- ifelse(x >= 0, x ^ 2, 0)
plot(x, y, type="l")
#or
x <- seq(-10, 10, by = 0.01)
y <- x ^ 2
y[x < 0] <- 0
plot(x, y, type="l")

#Question 3
#(This is a different way to use switch than in the notes
# - here the x is integer while in the notes it was string)
x <- ceiling(runif(1) * 3)
res <- switch(x, "rainy", "hurricane", "hailstones")
cat(res, "\n")
#or
forecast <- c("rainy", "hurricane", "hailstones")
x <- ceiling(runif(1) * 3)
cat(forecast[x], "\n")

#better solution, suggested by a student in a previous incarnation of this module
x <- c("rainy", "hurricane", "hailstones")
sample(x, 1)

#Question 4
forecast <- c("rainy", "hurricane", "hailstones")
n.forecasts <- 0
repeat {
  x <- ceiling(runif(1) * 3)
  cat(forecast[x], "\n")
  if(x == 1) break
  n.forecasts <- n.forecasts + 1
}
cat("Number of foreasts before first rainy =", n.forecasts, "\n")

#Question 5
forecast <- c("rainy", "hurricane", "hailstones")
n.forecasts <- 0
max.forecasts <- 4
repeat {
  x <- ceiling(runif(1) * 3)
  cat(forecast[x], "\n")
  if((x == 1) | (n.forecasts == max.forecasts - 1)) break
  n.forecasts <- n.forecasts + 1
}
if(x == 1) {
  cat("Number of foreasts before first rainy =", n.forecasts, "\n")
} else {
  cat("Reached maximum number of", max.forecasts, "forecasts without getting rainy\n")
}

#Question 6
x <- matrix(runif(12), 4, 3)
#Slow way
n <- dim(x)[1]
res <- numeric(n)
for(i in 1:n) {
  res[i] <- sum(x[i, ])
}
res
#Faster
apply(x, 1, sum)
#Even faster way to do it
rowSums(x)


#---------------------------------------------------------------------------------
#Part 2

forecast <- function() {
#Purpose: Call this function once to get a weather forecast
#Inputs: none
#Outputs: character string giving forecast
  x <- ceiling(runif(1) * 3)
  res <- switch(x, "rainy", "hurricane", "hailstones")
  return(res)
}

n <- 10
forecasts <- numeric(n)
for(i in 1:n) {
  forecasts[i] <- forecast()
}
print(forecasts)

forecast.v2 <- function() {
#Purpose: One line version of previous function
#Inputs: none
#Outputs: character string giving forecast
  return(switch(sample(1:3, 1), "rainy", "hurricane", "hailstones"))
}

forecast.v3<-function() {
#Purpose: Another (not so neat) one line version of previous function
#Inputs: none
#Outputs: character string giving forecast
  return(switch(ceiling(runif(1, max=3)), "rainy", "hurricane", "hailstones"))
}

#Question 2

forecast<-function(n) {
#Purpose: Call this function to get n weather forecasts
#Inputs: n - number of weather forecasts
#Outputs: vector of weather forecasts
  res <- character(n)
  for(i in 1:n){
    #create each forecast
    x <- sample(1:3, 1)
    res[i] <- switch(x, "rainy", "hurricane", "hailstones")
  }
  return(res)
}

forecast.v2 <- function(n) {
#Purpose: Call this function to get n weather forecasts
#Inputs: n - number of weather forecasts
#Outputs: vector of weather forecasts
#Implementation note - this is a faster, vectorized version of the previous function
  res <- character(n)
  x <- sample(1:3, n, replace = TRUE)
  res[x == 1] <- "rainy"
  res[x == 2] <- "hurricane"
  res[x == 3] <- "hailstones"
  return(res)
}

#Question 3

plot.quadratic <- function(limits = c(-10, 10)) {
#Purpose: Plots the quadratic function within limits
#Inputs: limits - a vector of length 2 containing the limits for plotting
#Outputs: A plot, but nothing else
  
  #Error checking on inpus should go here!
  
  #Do the plot
  x <- seq(limits[1], limits[2], length=200)
  y <- x ^ 2
  plot(x, y, type='l')

  #Return nothing
  invisible(NULL)
}

#Question 4

plot.function<-function(limits = c(-10, 10), fn){
#Purpose: Plots the quadratic function within limits
#Inputs: 
#  limits - a vector of length 2 containing the limits for plotting
#  fn - a function that accepts a vector of inputs (the x axis) and 
#        returns a vector of outputs (the y axis)
#Outputs: 
#  A plot, but nothing else
  
  #Error checking on inputs should go here!
  
  #Do the plot
  x <- seq(limits[1], limits[2], length = 200)
  y <- fn(x)
  plot(x, y, type='l')
  
  #Return nothing
  invisible(NULL)
}

#Note in the above it would be nicer to have the fn argument first as it does not have
# a default

#try it out
plot.function(fn = sin)

#try with a user-defined function
my.fn <- function(x) {
  return(x + x ^ 2 + x * sin(x))
}
plot.function(c(-10,5), fn = my.fn)

#a student suggested that curve does the same thing:
curve(my.fn, -10, 5)
#You could put that into a function if you like, but it's 
# hardly worth it:
plot.function.v2 <- function(limits = c(-10, 10), fn) {
#version of the above function that uses curve
#Error checking on inputs should go here!
  
  curve(fn, limits[1], limits[2])
  
  #Return nothing
  invisible(NULL)
}
