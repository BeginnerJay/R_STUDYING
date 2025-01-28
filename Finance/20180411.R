Mvec <- seq(5000,100000,5000)
for (i in 1:20) {
  x = runif(Mvec[i],0,3)
  y = runif(Mvec[i],0,9)
  x^2<y?
}
bond_price <- function(T,c,y,m){
  r <- 1/(1+y/m)
  n <- T*m
  dfs <- r^(1:n)
  cfs <- c(rep(c/m,n-1),c/m+1)
  ans <- sum(dfs*cfs)
}

Tvec <- seq(1 : 9)
yvec <- seq(1:9)*0.1


for (i in 1:9) {
  for (j in 1:9) {
    Result[j,i] <- bond_price(Tvec[i],c,yvec[j],m)
  }
}









