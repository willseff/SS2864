means <- function(x){
  mean <- mean(x)
  geometric.mean <- reduce(x,`*`)^(1/length(x))
  
  return(list(mean = mean,
              geometric.mean = geometric.mean))
}

means(4:7)

col.means <- function(x){
  sapply(as.data.frame(x),means)
}

matrix <- matrix(c(1,2,3,4,5,6,7,8,9),ncol=3)

col.means(matrix)


eqn <- function(x,y){
  return(x^2 + 2*y^2 + 4*x*y)
}

cross.func <- function(x,y){
  
  c <- expand.grid(x,y)
  return(matrix(mapply(function(x,y) x^2 + 2*y^2 + 4*x*y,c['Var1'],c['Var2']),ncol=length(y)))
}


cross.func(c(1:5),c(1:5))
cross.func(1:3, 2:5)

expand.grid(1:5,1:5)


