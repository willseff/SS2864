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
hist(log(Boston$crim))


# rad variable (integer)

Boston$tax
unique(Boston$rad)
# plot comparing two features

dev.off()
p <- ggplot(data=Boston)

p + geom_point(aes(x=tax, y=rad)) 

summary(lm(data$tax~data$rad))

# correlation plot
dev.off()
corrplot(cor(Boston), method='color', type='lower', diag=FALSE, cl.pos="r", 
         title = , addCoef.col = "black",
         tl.srt = 90, number.cex = .6)
mtext("Correlation Plot of Boston Dataset", at=6, line=2, cex=1)

# quick anova analysis 
summary(aov(data=Boston, medv ~ .^2 ))

# interaction plots
dev.off()
bins <- c('low','medium','high')

Boston.c <- data.frame(apply(Boston,2,function (x) cut(x,3,bins))) 

attach(Boston.c)
interaction.plot(factor(indus,level=c('low','medium','high')) ,lstat,Boston$medv)

interaction.plot(factor(indus,level=c('low','medium','high')) ,lstat,Boston$medv)
age.c

