laplace <- function(n){
  u = runif(n)
  x = rexp(n)
  data <- data.frame(u,x)
  apply(data,1, function(v){
    if(v[1] < 0.5)
      return(v[2])
    else
      return(-v[2])
  })
  
}
plot(density(laplace(10000)))


normal_laplace <- function(n){
  sapply(1:n, function(n){
    nonAccepte <- TRUE
    while({
      #On accepte si b1 <= b2
      # donc on continue tant que b2 < b1
      u <- runif(1)
      x1 <- laplace(1)
      
      b1 <- sqrt(1/2*pi) * exp(1/2 - abs(x1))
      b2 <- 1/sqrt(2*pi) * exp(-x1^2/2)
      (u*b1 <= b2)
    }){}
    x1
  })
}
plot(density(normal_laplace(10000)))

normal_laplace2 <- function(n){
  sapply(1:n, function(n){
    nonAccepte <- TRUE
    while({
      #On accepte si b1 <= b2
      # donc on continue tant que b2 < b1
      u <- runif(1)
      x1 <- laplace(1)
      
      (abs(x1) -1)^2 > -2*log(u)
    }){}
    x1
  })
}
plot(density(normal_laplace2(10000)))
