library(dplyr)
library(Rlab)

# Question 1

percentile <- function(x,y) {
  # x is the population distribution
  # y is the evaluation point
  # only works for one evaluation point
  
  (max(which(sort(append(x,y))  %in% y))-1)/length(x)
}

percentile.v <- function(x,y){
  # x is the distribution
  # y is the evaluation point
  # works for multiple evaluation points 
  return(sapply(y,function(g) percentile(x,g)))
}


# test cases
percentile.v(runif(100000),0.5)
percentile.v(runif(100000),c(1,2,3,4,5,6,7,8,9)/10)
percentile.v(c(0,1),c(0,1))
percentile.v(c(1,2,3,4,5),c(0,1,1.1,0.9))

# Question 2

# function 
matrix.multiply <- function(a,b) {
  # takes in two matrices a and b
  if (nrow(a)!=ncol(b)){
    return('Invalid')
  }
  
  c <- rep(seq(nrow(a)),nrow(a))
  
  return(t(matrix(mapply(function(x,y) sum(a[x,]*b[,y]),sort(c),c),ncol=nrow(a))))
  
}

# test cases 

# 2x2 case
a <- matrix(data=c(1,2,3,4,5,6,7,8),nrow=2)
b <- matrix(data=c(9,8,7,6,5,4,3,2),ncol=2)

matrix.multiply(a,b)
a %*% b

# 3x3 case
a <- matrix(data=c(1,2,3,4,5,6,7,8,1,2,3,8),nrow=3)
b <- matrix(data=c(9,8,7,6,5,4,3,2,1,2,3,8),ncol=3)

matrix.multiply(a,b)
a%*%b

# undefined case
a <- matrix(data=c(1,2,3,4,5,6,7,8,1,2,3,8),nrow=2)
b <- matrix(data=c(9,8,7,6,5,4,3,2,1,2,3,8),ncol=3)

matrix.multiply(a,b)


# Question 3

set.seed(1337)

pi.estimate <- function(n){
  
  return(sum(sqrt(runif(n,-1,1)^2  + runif(n,-1,1)^2) < 1)/n *4)
  
}

pi.estimate(100)
pi.estimate(1000)
pi.estimate(10000)

# Question 4

# function to create mean of n bernoulli random variables 
# and return if 0.8 is within its confid interval 
sim <- function(n,z,p){
  
  dat <- rbinom(n,1,p)
  
  p_hat <- mean(dat)
  
  r <- z*sqrt(p_hat*(1-p_hat)/n)
  
  return(p_hat+r>=p & p_hat-r<=p)
 
}

rep.sim <- function(m,n,z,p){
  
  results <- replicate(m,sim(n,z,p))
  
  return(sum(results)/m)
}

set.seed(2333)
# (1)
rep.sim(100,20,1.96,0.8)

# (2)
rep.sim(100,100,1.96,0.8)

# 2 
plot(c(5:200),c(5:200) %>% sapply(function(x) rep.sim(500,x,1.96,0.8)),
     xlab='n', 
     main='Proportion of Confidence Invervals \nthat Include the True Value',
     ylab = 'Proportion')


