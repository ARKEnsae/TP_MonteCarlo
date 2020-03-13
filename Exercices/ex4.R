geometrique <- function(n, p){
  sapply(1:n, function(i){
    k=0
    while({
      u = runif(1)
      k = k+1
      
      u>p}){
      
    }
    k
  })
}
p = 0.7
geo <- geometrique(10000, p)
hist(geo)

t.test(t, mu = 1/p)

sd(t) - 1/p
# On peut regarder les moments, variance, 


var.test(t, (1-p)/p^2)
