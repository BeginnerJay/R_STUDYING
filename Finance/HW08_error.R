CallPrice_CRR <- function(S, K, r, T, sig, N, q = 0, euro=TRUE) {
  dt <- T/N
  u <- exp(sig*sqrt(dt))
  d <- 1/u
  stock_tree <- matrix(0,N+1,N+1)
  stock_tree[,1] <- S*d^(0:(nrow(stock_tree)-1))
  for (i in 2:nrow(stock_tree)) {
    stock_tree[i,2:i] <- stock_tree[(i-1),1:(i-1)]*u
  }
  
  dsc <- exp(r*dt)
  p <- ((exp((r-q)*dt)) - d)/(u - d)
  
  option_tree <- matrix(0,nrow(stock_tree),ncol(stock_tree))
  option_tree[nrow(stock_tree),] <- pmax(stock_tree[nrow(stock_tree),] - K,0) 
  # don't use max()!!
  # option_tree[nrow(stock_tree),] <- pmax(K - stock_tree[nrow(stock_tree),],0) <- put
  for (i in (nrow(stock_tree)-1):1) {
    option_tree[i,i:1] <- (option_tree[i+1, 1:i]*(1-p) + option_tree[i + 1, 2:(i + 1)]*p)/dsc
    if (euro==FALSE) {
      option_tree[i,1:i] <- pmax(option_tree[i,1:i], stock_tree[i,1:i]-K)
      # pmax(option_[i,1:i], k-stock_tree[i,1:i]) <- put
    }
  }
  return(option_tree[1,1])
  # T=옵션 만기, K=옵션 행사가격, S=기초자산 가격, sig = 변동성, r = 이자율
  # nmb_p <-1 number of period
}
CallPrice_CRR(100,100,0.04,0.5,0.3,5)

CRR.Plot <- function(S,K,r,T,sig,N,beg,end,interval) {
  Result_2 <- data.frame(N = NA, CRR = NA, BS = NA, Error = NA)
  j <- 1
  for (i in seq(beg,end,interval)) {
    Result_2[j,1] <- i
    Result_2[j,2] <- CallPrice_CRR(S, K, r, T, sig, i)
    Result_2[j,3] <- CallPrice_BS(S, K, r, T, sig)
    Result_2[j,4] <- CallPrice_CRR(S, K, r, T, sig, i) - CallPrice_BS(S, K, r, T, sig)
    j <- j+1
  }
  par(mfrow = c(2,1))
  plot(Result_2$CRR, main = 'CRR_PRICE', type = 'l', xlab = 'N', ylab = 'Price', col = 'green')
  abline(h = Result_2$BS, col = 'red', lty = 2)
  plot(Result_2$Error, main = 'CRR_ERROR', type = 'o', xlab = 'N', ylab = 'Error', col = 'green')
  abline(h = 0, col = 'red', lty = 2)
}
CRR.Plot(100,100,0.04,0.5,0.3,5,1,400,1)

CallPrice_CRR.1 <- function(S, K, r, T, sig, N = 5, type='call') {
  q <- 0 # 배당
  dt <- T/N # delta T
  u <- exp(sig*sqrt(dt)); d = 1/u # u,d값 설정
  p <- ((exp((r-q)*dt)) - d)/(u - d) # 위험중립확률 설정
  
  stock_tree <- matrix(0, nrow=N+1, ncol=N+1) # 기초자산이 어떻게 변하는지에 대한 행렬 생성
  for (i in 1:(N+1)) {
    for (j in 1:i) {
      stock_tree[i,j] <- S * u^(j-1) * d^((i-1)-(j-1))
    }
  }
  option_tree = matrix(0, nrow=nrow(stock_tree), ncol=ncol(stock_tree))
  if(type == 'put') {
    option_tree[nrow(option_tree),] = pmax(K - stock_tree[nrow(stock_tree),], 0)
  } else {
    option_tree[nrow(option_tree),] = pmax(stock_tree[nrow(stock_tree),] - K, 0)
  }
  for (i in (nrow(stock_tree)-1):1) {
    for(j in 1:i) {
      option_tree[i, j] = ((1-p)*option_tree[i+1,j] + p*option_tree[i+1,j+1])/exp(r*dt)
    }
  }
  return(option_tree[1,1])
}  
CallPrice_CRR.1(100,100,0.04,0.5,0.3,N = 5)

CRR.Plot.1 <- function(S,K,r,T,sig,N,beg,end,interval) {
  Result_1 <- data.frame(N = NA, CRR = NA, BS = NA, Error = NA)
  j <- 1
  for (i in seq(beg,end,interval)) {
    Result_1[j,1] <- i
    Result_1[j,2] <- CallPrice_CRR.1(S, K, r, T, sig, i)
    Result_1[j,3] <- CallPrice_BS(S, K, r, T, sig)
    Result_1[j,4] <- CallPrice_CRR.1(S, K, r, T, sig, i) - CallPrice_BS(S, K, r, T, sig)
    j <- j+1
  }
  par(mfrow = c(2,1))
  plot(Result_1$CRR, main = 'CRR_PRICE', type = 'l', xlab = 'N', ylab = 'Price', col = 'green')
  abline(h = Result_1$BS, col = 'red', lty = 2)
  plot(Result_1$Error, main = 'CRR_ERROR', type = 'l', xlab = 'N', ylab = 'Error', col = 'green')
  abline(h = 0, col = 'red', lty = 2)
}
CRR.Plot.1(100,100,0.04,0.5,0.3,5,1,400,1)

CallPrice_CRR.1 <- function(S, K, r, T, sig, N, type) {
  q = 0 # 배당
  dt = T/N
  u = exp(sig*sqrt(dt)); d = 1/u
  p = ((exp((r-q)*dt)) - d)/(u - d)
  
  tree = matrix(0, nrow=N+1, ncol=N+1)
  for (i in 1:(N+1)) {
    for (j in 1:i) {
      tree[i,j] = S * u^(j-1) * d^((i-1)-(j-1))
    }
  }
  
  option_tree = matrix(0, nrow=nrow(tree), ncol=ncol(tree))
  if(type == 'put') {
    option_tree[nrow(option_tree),] = pmax(K - tree[nrow(tree),], 0)
  } else {
    option_tree[nrow(option_tree),] = pmax(tree[nrow(tree),] - K, 0)
  }
  for (i in (nrow(tree)-1):1) {
    for(j in 1:i) {
      option_tree[i, j] = ((1-p)*option_tree[i+1,j] + p*option_tree[i+1,j+1])/exp(r*dt)
    }
  }
  return(option_tree[1,1])
}  
CallPrice_CRR.1(100,100,0.04,0.5,0.3,5,'call')
