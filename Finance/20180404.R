rm(list=ls())
f <- function(x) x^2
k <- 5
f1 <- function(x) 2*x
tol <- 1e-10
x0 <- k/2

x<- x0
iter <- 0
while(abs(f(x)-k)>tol) {
  x <- x - (f(x)-k)/f1(x)
  iter <- iter + 1
}
x; x^2; iter

set.seed(1)
I <- 2
r <- I/2
M <- 100000000
x <- runif(M,-r,r)
y <- runif(M,-r,r)

in.prob <- sum(sqrt(x^2+y^2)<=r)/M # 사각형의 넓이 비율 분에 원 안에 들어가는 비율
app.pi <- in.prob*(I^2)
app.pi; pi
