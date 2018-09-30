#edf plot
#  use beta distribution as an example
par(mfrow=c(2,2))
a <- 3
b <- 1.5
n <- 30
x <- seq(0, 1, length=100)
f <- dbeta(x, a, b)
plot(x, f, type="l", main="pdf")
F <- pbeta(x, a, b)
plot(x, F, type="l", main="CDF")
X <- rbeta(n,a,b)
hist(X, main=paste(n,"samples"), xlim=c(0,1))
rug(X)
eF <- ecdf(X)
plot(eF, main="EDF", verticals=TRUE, xlim=c(0,1))
par(mfrow=c(1,1))

#bootstrap resample of martin
martin <- c(0.13,-0.01,-0.01,0.42,-0.02,0.01,0.09,0.03,0.04,0.06,0.12,0.03)
nreps <- 1000
how.often <- matrix(NA, nrow=nreps, ncol=length(martin))
for(i in 1:nreps) how.often[i,] <- sample(martin,length(martin),replace=TRUE)
table(how.often)

boot.martin <- function(data, b, alpha, type="nonp") {
#  Purpose:  compute 95% CI endpoints for dataset
#  Input: dataset, number of replicates, type in ("nonp", "par-norm", "par-t")
#  output: histogram with CI endpoints
  boot <- matrix(NA, nrow=b+1, ncol=length(data))
  m <- mean(martin)
  sd <- sd(martin)
  title <- paste(b, type, "bootstraps", sep=' ')
  if (type == "nonp") {
    for(i in 1:b) {boot[i,] <- sample(data,length(data),replace=TRUE) }
  } else {
    if (type=="par-norm") {
    for(i in 1:b) {boot[i,] <- rnorm(length(data),m,sd) }
    } else {
      if (type=="par-t") {
      for(i in 1:b) {boot[i,] <- (rt(length(martin),length(martin)-1)*sd)+m }
      } else {
        stop("Bootstrap type !in 'nonp', 'par-norm', 'par-t")
      }}}
      
  n.elements <- b+1
  boot[n.elements, ] <- data
  mean.boot <- apply(boot,1,mean)  # rowMeans would work equally well
  hist(mean.boot, main=title, freq=FALSE)
  m <- mean.boot[1:b]
  x <- order(m)
  lower.quantile <- floor(alpha/2 * n.elements)
  upper.quantile <- ceiling((1-alpha/2) * n.elements)
  endpoints <- c(m[x[lower.quantile]], m[x[upper.quantile]])
  abline(v=endpoints[1], lty=2)
  abline(v=endpoints[2], lty=2)
  return(endpoints)
}

boot.martin(martin, b=99, alpha=0.05)
boot.martin(martin, b=999, alpha=0.05, 'par-norm')
boot.martin(martin, b=999, alpha=0.05, 'par-t')
boot.martin(martin, b=999, alpha=0.05, 'para-norm')

