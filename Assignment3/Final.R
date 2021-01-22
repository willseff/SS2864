# Question 1
set.seed(2864)
x <- rnorm(1000,1,1)

z <- x[x>3]
head(z,10)

y <- ifelse(x-1>0, x, 0)
head(y,20)

# Question 2
mat_ones <- function(n){
  
  m <- matrix(1,ncol=n,nrow=n)
  m[upper.tri(m)] <- 0
  return(apply(m, 2, rev))
  
}

mat_ones(10)

# Question 3

cumsum <- function(x){
  cumsum.vector <- rep(0,length(x))
  
  for (i in x){
    cumsum <- reduce(x[c(1:i)],`+`)
    cumsum.vector[i] <- cumsum
  }
  return(cumsum.vector)
}

cumsum(c(1:5))

# Question 4

library(dplyr)

# 4a
summarise(group_by(iris,Species),'Mean' = mean(Sepal.Length))

summarise(group_by(iris,Species),'Standard Deviation' = sd(Sepal.Length))

summarise(group_by(iris,Species),'Median' = median(Sepal.Length))

# 4b

boxplot(iris$Sepal.Width~iris$Species)

# 4c

g <- ggplot(data=iris)

g + geom_boxplot(aes(x=Species,y=Sepal.Width))

# 4d

g + geom_density(aes(Sepal.Length, color=Species, fill=Species, alpha=0.5))

#4e 

g + geom_point(aes(x=Sepal.Length,y=Sepal.Width, color=Species)) + 
  geom_smooth(aes(x=Sepal.Length,y=Sepal.Width, color=Species), method = 'lm')

#4f

group_by(iris[,c('Sepal.Length','Sepal.Width')],iris$Species) %>% 
  summarise(Correlation.Sepal.Length.Width=cor(Sepal.Length,Sepal.Width))


# Question 5

# 5a
prop.table(apply(HairEyeColor,1,sum))

# 5b
barplot(prop.table(apply(HairEyeColor,c('Hair','Sex'),sum),2))

# Question 6
library(MASS)

data <- birthwt

data$smoke <- ifelse(birthwt$smoke == 1, 'yes','no')
data$low <- ifelse(birthwt$low == 1, 'weight_low','weight_n_low')

sapply(data[,c('bwt','smoke','low')], summary)

table(data$smoke,data$low, dnn=c('Smoker','Low Weight'))

par(mfrow=c(1,2))

barplot(table(data$smoke,data$low),xlab='Smoker?', ylab='Birthweight Low?')

barplot(prop.table(table(data$smoke,data$low),2))

# Question 7

set.seed(2864)
x <- rnorm(20,1,1)
y <- rnorm(20,1.1,1.1)

x_bar <- mean(x)
y_bar <- mean(y)

sx <- sd(x)
sy <- sd(y)

nx <- length(x)
ny <- length(y)

s_delta <- sqrt((sx^2/nx) + (sy^2/ny))

t <- ( x_bar - y_bar ) / s_delta
t

t.test(x,y)$statistic


cumsum(c(1:5))
