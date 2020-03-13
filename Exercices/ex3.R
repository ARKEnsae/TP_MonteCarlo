box_muller_ameliore <- function(n){
  result <- sapply(1:n, function(i){
    
    while({
      u <- runif(2,min = -1, max = 1)
      S = u[1]^2 + u[2]^2
      S > 1
      }){}
    x = u[1] * sqrt(-2 * log(S)/S)
    y = u[2] * sqrt(-2 * log(S)/S)
    c(x, y)
  })
  result <- data.frame(t(result))
  colnames(result) <- c("X", "Y")
  result
}

box_muller <- function(n){
  result <- sapply(1:n, function(i){
    u <- runif(2)
    x = sinpi(2*u[1]) * sqrt(-2 * log(u[1]))
    y = cospi(2*u[2]) * sqrt(-2 * log(u[1]))
    c(x, y)
  })
  result <- data.frame(t(result))
  colnames(result) <- c("X", "Y")
  result
}
simul_box_muller_amelio <- box_muller_ameliore(1000000)
simul_box_muller <- box_muller(1000000)

microbenchmark::microbenchmark(`Version améliorée` = {
  box_muller_ameliore(10000)
},
`Version classique` = {
  box_muller(10000)
},times = 100)

profvis::profvis({
  box_muller_ameliore(1000)
  simul_box_muller <- box_muller(1000000)
})
plot(simul_box_muller)
plot(density(simul_box_muller$Y))
