# 초기 환경 설정
rm(list=ls())
rm(list=lsf.str())
rm(list=setdiff(ls(), lsf.str()))
dev.off()   
cat("\014")

### 문제 a

# 블랙 - 숄즈 공식
# 블랙 - 숄즈 공식은 기초자산인 주식을 pnorm(d1)주만큼 매입하고, K*exp(-r*T)*pnorm(d2)만큼 차입하면 콜옵션 1개를 매입한 것과 동일한 것임을 의미한다. 
CallPrice_BS <- function(S, sig, r, K, T, Dt = 0, q = 0)
{
  # T=옵션 만기, K=옵션 행사가격, S=기초자산 가격, sig = 변동성, r = 이자율, Dt = t 시점의 확정배당금, q = 배당수익율
  # pnorm <- -무한 에서 d1까지의 누적확률값
  d1 = (log(S/K) + (r+0.5*sig^2)*T) / (sig*sqrt(T))
  d2 = d1 - sig*sqrt(T)
  BSCall = (S/exp(-q*T)-Dt/exp(-r*T))*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
  return(BSCall)
}

#함수 사용
CallPrice_BS(100, 0.3, 0.04, 100, 0.5)

# CRR 이항 옵션가격 결정모형
# u = exp(sig*sqrt(dt)), ud = 1 , sig = volatility, dt = T/N 가정
# 위험중립확률 p = ((1 + Rf) - d)/(u - d) <- 이산
# 위험중립확률 p = ((exp((Rf-q)*dt)) - d)/(u - d) <- 연속, 배당수익률을 고려함

# ex1) sig = 0.3, Rf = 0.05, dt = 1일 때, 1기간 후 주식 가격이 상승할 위험중립확률 : 0.5097
u = exp(0.3*sqrt(1)); d = 1/u; u*d; p <- ((exp((0.05-0)*1)) - d)/(u - d)
# ex2) sig = 0.3, Rf = 0.04, dt = 0.1일 때, 1기간 후 주식 가격이 상승할 위험중립확률 : 0.4973
u = exp(0.3*sqrt(0.1)); d = 1/u; u*d; p <- ((exp((0.04-0)*0.1)) - d)/(u - d)

# 시장 변수(S0 = S, sig, Rf = r) / 옵션 조건(K, T) / Tree 변수 (N, u, d, q)
CallPrice_CRR <- function(S, sig, r, K, T, N) {
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
  option_tree[nrow(option_tree),] = pmax(stock_tree[nrow(stock_tree),] - K, 0) # put이면 반대로 해주기
  for (i in (nrow(stock_tree)-1):1) {
    for(j in 1:i) {
      option_tree[i, j] = ((1-p)*option_tree[i+1,j] + p*option_tree[i+1,j+1])/exp(r*dt) # Backwardation
    }
  } # 옵션이 기간이 지남에 따라 가격이 어떻게 바뀌는지를 알려주는 행렬 생성
  return(option_tree[1,1]) # Backwardation한 결과인 현재 가격을 반호
}  
CallPrice_CRR(100, 0.3, 0.04, 100, 0.5, N = 5)



CRR.Plot <- function(S, sig, r, K, T, beg, end, interval) {
  Result <- data.frame(N = NA, CRR = NA, BS = NA, Error = NA)
  j <- 1
  for (i in seq(beg,end,interval)) {
    Result[j,1] <- i
    Result[j,2] <- CallPrice_CRR(S, sig, r, K, T, i)
    Result[j,3] <- CallPrice_BS(S, sig, r, K, T)
    Result[j,4] <- CallPrice_CRR(S, sig, r, K, T, i) - CallPrice_BS(S, sig, r, K, T)
    j <- j+1
  }
  par(mfrow = c(2,1))
  plot(Result$CRR, main = 'CRR_PRICE', type = 'l', xlab = 'N', ylab = 'Price', col = 'green')
  abline(h = Result$BS, col = 'red', lty = 2, xaxs = "i")
  plot(Result$Error, main = 'CRR_ERROR', type = 'l', xlab = 'N', ylab = 'Error', col = 'green')
  abline(h = 0, col = 'red', lty = 2, xaxs = "i")
}
CRR.Plot(100, 0.3, 0.04, 100, 0.5, 1, 400, 1)
