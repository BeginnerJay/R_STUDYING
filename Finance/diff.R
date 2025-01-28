rm(list = ls())

N <- 5; T <- 0.5; sig <- 0.3; S <- 100; q <- 0; r <- 0.04; K <- 100

dt <- T/N; u <- exp(sig*sqrt(dt)); d <- 1/u; p = ((exp((r-q)*dt)) - d)/(u - d); dsc <- exp(r*dt)

stock_tree_1 <- matrix(0, N+1, N+1); stock_tree_1
stock_tree_1[,1] <- S*d^(0:(nrow(stock_tree_1)-1));stock_tree_1
for (i in 2:nrow(stock_tree_1)) {
  stock_tree_1[i,2:i] <- stock_tree_1[(i-1),1:(i-1)]*u
};stock_tree_1



stock_tree_2 = matrix(0, nrow=N+1, ncol=N+1)
for (i in 1:(N+1)) {
  for (j in 1:i) {
    stock_tree_2[i,j] = S * u^(j-1) * d^((i-1)-(j-1))
  }
};stock_tree_2

stock_tree_1;stock_tree_2




option_tree_1 <- matrix(0,nrow(stock_tree_1),ncol(stock_tree_1));option_tree_1
option_tree_1[nrow(stock_tree_1),] <- pmax(stock_tree_1[nrow(stock_tree_1),]-K,0);option_tree_1 # don't use max()!! / opt[nt,] <- pmax(k-st[nt,],0) <- put
for (i in (nrow(stock_tree_1)-1):1) {
  option_tree_1[i,i:1] <- (option_tree_1[i + 1, 1:i]*(1-p) + option_tree_1[i + 1, 2:(i + 1)]*p)/dsc
};option_tree_1

option_tree_2 = matrix(0, nrow=nt, ncol=ns);option_tree_2
option_tree_2[nt,] <- pmax(stock_tree_2[nt,] - K, 0);option_tree_2
for (i in (nt-1):1) {
  for(j in 1:i) {
    option_tree_2[i, j] = ((1-p)*option_tree_2[i+1,j] + p*option_tree_2[i+1,j+1])/dsc
  }
};option_tree_2
