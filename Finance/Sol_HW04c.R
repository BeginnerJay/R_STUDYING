# Bisection Method를 이용한 이표채권의 YTM 계산

rm(list=ls()) #clearing of datasets in Rstudio Environment
rm(list=lsf.str())
rm(list=setdiff(ls(), lsf.str()))
dev.off()     #clearing of plots in Rstudio
cat("\014")   #clearing of console in Rstudio


## 문제
F.val <- 100    #액면
T.val <- 3      #(잔존)만기
c.val <- 0.1    #이표율
m.val <- 1      #연 이자지급 횟수 
P.val <- 108.22 #채권가격

bond.price <- function(T,c,y,m){ #채권가격 계산함수
  #Output: bond price. Assume notional is 1.
  #Inputs:
  # T = maturity(year) 
  # c = coupon rate
  # y = YTM
  # m = frequency. Default is Semi-annual.
  
  r <- 1/(1+y/m)
  n <- T*m       #총 이자부리 횟수
  dfs <- r^(1:n) #할인계수 벡터
  cfs <- c(rep(c/m,n-1), c/m+1)
  ans <- sum(dfs*cfs)
  #  ans <- t(dfs)%*%cfs #결과 동일
}

f <- function(y) { #목적함수
  F.val*bond.price(T=T.val, c=c.val, y, m=m.val) #R에서 변수선언은 기본적으로 전역변수임을 이용
}
k <- P.val      #목적값

## Bisection Method
# 사전 세팅
tol <- 1e-5 #f(x) 기준의 허용오차
maxiter <- 50
lower0 <- 0.01; #lower bound
upper0 <- 0.20  #higher bound
Result <- matrix(NA,maxiter,2) #x의 수렴과정을 저장하는 행렬. NA로 초기화


# 알고리즘 실행
lower <- lower0
upper <- upper0
x <- (lower + upper)/2
iter <- 1
Result[1,] <- c(iter, x) #초기값 저장
while(abs(f(x)-k)>tol & iter<maxiter){ #미지수는 x. 나머지 입력변수는 주어진 값.
  lower <- ifelse((f(upper)-k)*(f(x)-k)<0, x, lower)
  upper <- ifelse((f(lower)-k)*(f(x)-k)<0, x, upper)
  x <- (lower + upper)/2

  iter <- iter + 1
  print(paste0("iter=",iter, ", x=",x)) #진행상태 출력
  Result[iter,] <- c(iter, x) #진행상태 저장
}
x
print(f(x)) #검산


## 그래프로 결과 살펴보기
Result1 <- Result[is.na(Result[,1])==F,] #iteration 돌지 않은 부분 버리기
plot(Result1, type='l', main='Bisection Method', xlab='iteration', ylab='YTM')
abline(h=x, col='red') #수렴값 겹쳐 그리기

