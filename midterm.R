set.seed(2864)
x <- rnorm(1000,1,2)
# we know this has a skew of 0

numerator <- sum((x - mean(x))^3)/length(x)
numerator
denominator <- (sum((x - mean(x))^2)^(3/2))/length(x)
denominator

4^(3/2)

numerator/denominator

set.seed(2864)
x <- rnorm(1000,1,2)
skew_1 <- mean((x-mean(x))^3)
skew_1
skew_2 <- sd(x)^3
skew_2
skew <- skew_1/skew_2
skew