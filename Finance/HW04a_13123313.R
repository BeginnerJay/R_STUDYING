rm(list=ls())
set.seed(13123313)
f <- function(x) x^2
a <-0; b <- 3; c <- f(0); d <- f(3)
a;b;c;d
n <- 10000
x <- runif(n,a,b)
x
y <- runif(n,c,d)
y
A <- (sum(y<f(x))/n)*((b-a)*(d-c))
A
integrate(f,a,b)
# 한 번씩 하기는 성공

# function()으로 뽑기 <- 성공했지만 어떻게 행렬로??

g <- function(i) {
  set.seed(13123313)
  x = runif(i,0,3)
  y = runif(i,0,9)
  app = ((sum(y<f(x)))/i)*27
  print(app)
}
g(5000)

# dataframe에 집어넣기

Result <- data.frame(trial = NA, app = NA)
j <- 1
for (i in seq(5000,100000,5000)) {
  set.seed(13123313)
  Result[j,1] <- i
  app <- g(i)
  Result[j,2] <- app
  j <- j+1
}
View(Result)
plot(Result)
