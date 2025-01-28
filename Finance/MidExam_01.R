# 초기 환경 설정
rm(list=ls())
rm(list=lsf.str())
rm(list=setdiff(ls(), lsf.str()))
dev.off()   
cat("\014")

# 한 번씩 하기
set.seed(13123313)
f <- function(x) x^3 + 3*x^2
ff <- expression(x^3 + 3*x^2)
D(ff,'x')
polyroot(c(0,6,3)) # -2,0에서 f(X)가 극값 갖는다.
f(-2);f(0) # f(0)에서 극소값 갖는다.
f(-1);f(0);f(3) # 구간의 양 끝값과 비교 -> f(0)이 구간 내에서 최소값이다.
a <- -1; b <- 3; c <- f(0); d <- f(3)
a;b;c;d
n <- 5000
x <- runif(n,a,b)
x
y <- runif(n,c,d)
y
Approximation <- (sum(y<f(x))/n)*((b-a)*(d-c))
Approximation
Actual <- integrate(f,a,b)$value
Actual

# function()으로 뽑기 후 데이터프레임으로 만들기

g <- function(i) {
  a <- -1; b <- 3; c <- f(0); d <- f(3)
  set.seed(13123313)
  x = runif(i,-1,3)
  y = runif(i,0,54)
  app = ((sum(y<f(x)))/i)*((b-a)*(d-c))
  print(app)
}
g(5000)

# dataframe에 집어넣어 비교하기

Result <- data.frame(trial = NA, app = NA)
j <- 1
for (i in seq(5000,1000000,5000)) {
  set.seed(13123313)
  Result[j,1] <- i
  app <- g(i)
  Result[j,2] <- app
  j <- j+1
}
View(Result)

# 그래프로 비교하기
plot(Result, type='b', main='Actual & Approximate', xlab='size of sample', ylab='value', col = 'blue') # 점점 더 참값으로 수렴하는 것을 보인다.
abline(h=Actual, col='red') # 참값으로 수평선 그어주기

