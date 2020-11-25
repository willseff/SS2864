install.packages('corrplot')

library(MASS)
library(corrplot)
library(ggplot2)
library(dplyr)

# simple preprofiling 
summary(Boston)
head(Boston)
dim(Boston)
sapply(Boston,class)

# histogram of all features
dev.off()
par(mfrow = c(4,4), mai = c(0.1, 0.6, 0.5, 0.1))

mapply(function(x,y) hist(x,main=y),
       Boston,
       colnames(Boston))

data <- Boston
# feature engineering / profiling of each variable
# crim variable

dev.off() 
hist(data$crim,breaks=200)
plot(Boston$crim,Boston$medv)

# apply log transformation
data$crim <- sapply(Boston[,'crim'],function (x) log(x))
hist(data$crim)

plot(data$crim,data$medv)

# drop black feature, unsure how to use parabolic transformation and uninvertible function
data$black <- NULL

# zn variable
unique(Boston$zn)
# 26 unique values
dev.off()

# rad variable (integer)

Boston$tax
unique(Boston$rad)

# correlation plot
dev.off()
corrplot(cor(Boston), method='color', type='lower', diag=FALSE, cl.pos="r", 
         title = , addCoef.col = "black",
         tl.srt = 90, number.cex = .6)
mtext("Correlation Plot of Boston Dataset", at=6, line=2, cex=1)

# quick anova analysis 
summary(aov(data=data, medv ~ .^2 ))

# interaction plots
dev.off()
bins <- c('low','medium','high')

Boston.c <- data.frame(apply(data,2,function (x) cut(x,3,bins))) 

attach(Boston.c)

ord <- function(x){
  return(factor(x,level=c('low','medium','high')))
}

par(mfrow = c(2,2), mar = c(4,4,3,3))
mtext("Interesting Interactions", at=0, line=1, cex=1)
interaction.plot(ord(indus), 
                 lstat,
                 Boston$medv,
                 xlab='indus')

interaction.plot(ord(crim),
                 ord(rm),
                 Boston$medv,
                 xlab='crim')

interaction.plot(ord(crim),
                 ord(lstat),
                 Boston$medv,
                 xlab='crim')

interaction.plot(ord(nox),
                 Boston$chas,
                 Boston$medv,
                 xlab='rad')

mtext("Interesting Interactions", at=0, line=18, cex=1)

