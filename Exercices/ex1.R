set.seed(15011933)
randu <- function(n, start = 1){
  init0 = as.double(start)
  init1 = init0 * 65539 %% (2^31)
  res = vector(length = n)
  res[1] = init0/65539
  res[2] = init0/65539
  for(i in seq_len(n-3)){
    res[i+2] = (6*res[i+1] - 9 * res[i]) %% 1
  }
  res
}

randu_x <- function(n, start = 1){
  init0 = as.double(start)
  init1 = init0 * 65539 %% (2^31)
  res = vector(length = n)
  res[1] = init0
  res[2] = init0
  for(i in seq_len(n-3)){
    res[i+2] = (6*res[i+1] - 9 * res[i]) %% (2^31)
  }
  res
}


simul <- randu(20000)
simul_x <- randu_x(20000)
head(simul,15)
?`%%`
library(zoo)
val = rollapply(simul, width = 3, function(x){
  if(0.5<=x[2] & x[2] <= 0.51){
    return(c(x[1], x[3]))
  }
})

plot(val, xlab = expression(u[i-2]), ylab = expression(u[i]))

#graph3d
toutes_donnees = rollapply(simul, width = 3, function(x){
  return(c(x[1],x[2], x[3]))
})

rgl::plot3d(toutes_donnees,col="red", xlab = expression(u[i-2]), ylab = expression(u[i-1]), zlab = expression(u[i])) 


sapply(simul_x[1:200], intToBits)
