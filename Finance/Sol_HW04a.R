# 몬테카를로 시뮬레이션을 통한 정적분 근사값 계산
# (손으로 적분할 수 없는 함수의 정적분을 계산할 수 있다.)

rm(list=ls()) #clearing of datasets in Rstudio Environment
rm(list=lsf.str())
rm(list=setdiff(ls(), lsf.str()))
dev.off()     #clearing of plots in Rstudio
cat("\014")   #clearing of console in Rstudio


## 문제
f <- function(x) x^2 #피적분 함수
xmin <- 0       #적분구간 시작값
xmax <- 3       #적분구간 끝값
ymin <- f(xmin) #적분구간에서 y의 최소값. f가 주어진 적분구간에서 증가함수이므로 이렇게 구함.
ymax <- f(xmax) #적분구간에서 y의 최대값. f가 주어진 적분구간에서 증가함수이므로 이렇게 구함.

true.val <- integrate(f,xmin,xmax)$value #참값. integrate(f,xmin,xmax)[[1]]도 OK.


## 근사값 계산 by Montecarlo Simulation
set.seed(1)   #매 시뮬레이션마다 동일한 난수가 생성되도록 seed number 고정
n.trial <- 20 #근사값 계산 횟수
n.sample <- 5000*seq(1:n.trial) #근사값 계산 시 생성하는 샘플 개수들의 벡터
app.val <- rep(0,n.trial)       #각 샘플사이즈에서 계산된 근사값을 저장하는 벡터 (0으로 초기화)
area.full <- (xmax-xmin)*(ymax-ymin) #전체 사각형의 넓이

for (i in 1:n.trial) {
  M <- n.sample[i]
  x <- runif(M, xmin, xmax)  #x좌표~U(xmin,xmax)
  y <- runif(M, ymin, ymax)  #y좌표~U(ymin,ymax)
  in.prob <- sum(y<f(x))/M   #넓이를 구하려는 영역 안으로 샘플 포인트가 놓인 비율
  app.val[i] <- in.prob*area.full #근사값
}


## 결과 정리
Result <- cbind(n.sample, app.val)
print(Result)


## 그래프로 결과 살펴보기
par(mfrow=c(1,2))
# 근사값 vs. 참값
plot(Result, type='l', main='True.Val  vs.  App.Val', xlab='sample size', ylab='area')
abline(h=true.val, col='red') #참값 겹쳐 그리기

# 절대오차 = |근사값-참값|
abserr <- abs(true.val - app.val)
plot(n.sample, abserr, type='l', main='Abs.Err', xlab='sample size', ylab='abs err')
