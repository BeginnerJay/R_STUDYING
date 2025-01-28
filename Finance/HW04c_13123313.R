rm(list=ls())

bond_price <- function(T,c,y,m){
  r <- 1/(1+y/m)
  n <- T*m
  dfs <- r^(1:n)
  cfs <- c(rep(c/m,n-1),c/m+1)
  ans <- sum(dfs*cfs)
}
x <- bond_price(3,0.1)