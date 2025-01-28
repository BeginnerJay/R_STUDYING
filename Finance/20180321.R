a <- 1
print(1)
print("Hello, world!")

a <- c(1, 2)
a
b <- c(3, 4)
b

c <- c(a, b)
c
d <- append(a,b)
d

e <- 1 : 9
e

f <- c(1:9, 0, 9:5)
f

g <- a + b
g

h <- seq(1:10)
h
i <- seq(from = 1, to = 10, by = 2)
i
j <- seq(1, 10, 2)

k <- seq(from = -3, to = 3, length = 100)
k

help("seq") # Usage에 숫자 적혀 있으면 default 값이라는 뜻

l <- rep(c(1,2,3), 5)
l

A <- matrix(c(1,2,3,4,5,6), nrow=3, ncol=2, byrow=FALSE) # 행 갯수 3개, 열 갯수 2개
A
B <- matrix(c(1,2,3,4,5,6), ncol=2, byrow=F)
B
C <- matrix(c(1,2,3,4,5,6), nrow=3, byrow=F)
C
# 왜 nrow나 ncol을 지웠는데도 똑같은 값이 나오지? -> byrow(가로로 정렬)을 F로 해놔서 세로로 정렬됨
D <- matrix(c(1,2,3,4,5,6), nrow=3, ncol=2, byrow=TRUE) # bycol은 없다
D
E <- matrix(c(1,2,3,4,5,6), 3, 2) # default값은 byrow=F / R에서 인자 이름은 생략할 수 있다
E

O <- matrix(0, 2, 2) # 영행렬
O
I <- matrix(1, 2, 2) # 모든 원소가 1인 행
I
help(diag)
I3 <- diag(x = 1, nrow = 3)  
I3
diag(3) # 값만 쳐도 3*3 정사각 단위행렬

a <- c(1, 2, 3) ; b <- c(3, 4, 5)
c <- cbind(a, b) # 열로 결합
c
class(c)
d <- rbind(a, b) # 행으로 결합
d

# 원소 indexing
a <- 1:9
a
a[2] # 2번째 원소를 찾아가기
a[c(3,5)] # 3, 5 번째 원소를 찾아가기
a[-2] # 2번째 원소만 빼고 찾아가기
a[c(-3,-5)] # 3,5 번째 원소만 빼고 찾아가

# 행렬 indexing
A <- matrix(a, 3)
A
A[2,1] # a21 찾아가기
A[3,2] # a32 찾아가기

B <- A[nrow = 1:2, ncol = 2:3] # 1행과 2행, 2열과 3열
B
A[1,] # 1행
A[,2] # 2열
A[c(-2), 2] # 2열에서 2행빼고

# 벡터 naming

V <- c(3, pi, 4)
V
v <- c(e, pi, 4)
v
names(V) <- c("num1", "num2", "num3")
V

# 행렬 naming
A <- matrix(c(75,80,78,92), 2, 2)
A
colnames(A) <- c("Man", "Women") # 열 이름짓기
rownames(A) <- c("영어", "수학") # 행 이름짓기
A
A["영어", "Man"]
A["Man"] # 반드시 행index와 열index를 같이 적어 주어야 인식함
A[, "Man"]

# 행렬의 연산
A <- matrix(c(5,6,7,8), 2)
B <- matrix(c(2,2,2,2), 2)
A %*% B # 행렬의 곱셈
A * B # 행렬의 원소별 곱셈
A %% B
A %% 3 # 나머지

T
F
!T
!F
as.numeric(T)
as.numeric(F)
T&T
T&F
F|F # 논리 연산

x <- c(T,T,F)
y <- c(TRUE,T,T)
x
y
rm(list=ls()) # list에 있는 것 다 지워 주는 함수
x*y
x+y
a <- 1>3
a

# 비교 연산
A <- matrix(c(1,5,8,3), 2)
B <- matrix(c(2,4,8,0), 2)
A==B;A!=B # 원소별로 같은지 다른지 비교
A>B; A<B

# 벡터와 행렬과 관련된 기본적 함수
length(A) # 행렬의 원소의 갯수
dim(A) # 행렬의 차원 (벡터에 사용하면 error)
nrow(A) # 행렬의 행의 갯수 (벡터에 사용하면 error)
ncol(A) # 행렬의 열의 갯수 (벡터에 사용하면 error)
a <- 1:6
dim(a) # 에러 나니까
dim(as.matrix(a)) # 6*1 행렬로 신분세

x <- rnorm(1000, 0, 1) #N(0,1)인 난수 생성. default값은 N(0,1)
x
mean(x) # x의 평균
sd(x) # x의 표준편차
quantile(a, 0.25) # a라는 벡터의 1분위수
quantile(x, 0.02)

dnorm(10000, 0, 1) # 확률밀도함수. 갯수,평균,표준편차 순서
pnorm(10000, 0, 1) # 누적확률함수. 갯수,평균,표준편차 순서
qnorm(0.1, 100, 10) # 분위수. 확률,평균,표준편차 순

hist(x, prob=T)
hist(x, prob=F)
help(hist)

x = seq(-3, 3, length=100); y = dnorm(x) # -3과 3 사이에 연속 1000개 / 그것의 확률밀도함수
plot(x,y, type="h") #type="h": 히스토그램

# 저수준 그래프 : lines() 함수 <- 그래프 겹쳐 그리기
x = seq(-6, 6, length=100)
y1 = dnorm(x) #N(0,1)
y2 = dnorm(x, 2, 1) #N(2,1)

plot(x, y1, type="l"); #N(0,1). type="l" : 실선
lines(x, y2, lty=2); #N(2,1) 겹쳐 그리기. lty=2 : 점선
text(-2, 0.2, "N(0,1)"); text(4, 0.3, "N(2,1)")

# 다중 그래프 그리기 : par() 함수의 mfrow=c(nrow,ncol) 인수
x = seq(-6, 6, length=100)
y1 = dnorm(x) #N(0,1)
y2 = dnorm(x, 2, 1) #N(2,1)

par(mfrow=c(1,2)) #다중 그래프를 그리기 위한 옵션
plot(x, y1, type="l") #N(0,1). type="l" : 실선
plot(x, y2, type="h") #N(2,1)

# (4) 제어문

# 1. 반복문 : for(Loop 변수 in 시작값:끝값) {실행문}
# 1부터 10까지 제곱합 계산하기
sum2 <- 0 #제곱합
for (i in 1:10) {sum2 <- sum2 + i^2}
sum2

# 2. 조건문 : if(Condition) {Condition이 True일 때의 실행문} else {Condition이 False일 때의 실행문}
# 1부터 10까지 홀수 제곱합 계산하기
sum3 <- 0
for (i in 1:10) {if(i%%2==1) {sum3 <- sum3 + i^2} else {sum3 <- sum3 + 0}}
for (i in 1:10) { if(i%%2==1) {sum2 <- sum2 +　i^2} }
sum3

# (5) 사용자 정의함수
# 형식 : 함수이름 <- function(입력변수1, 입력변수2, ... , 입력변수 n) {실행내용 return(결과값)}

# 예1) 두 개의 수 중 큰 값을 반환하는 함수

getMax <- function(x, y)
{
  if(x>=y) {return(x)} else {return(y)}
}
#함수 사용
x=1; y=3
getMax(x,y)

# 예2) 제곱합 계산함수

getSum2 <- function(n1, n2)
{
  sum2 = 0 #제곱합
  i = 0 #첨자
  for (i in n1:n2) { sum2 <- sum2 +　i^２ }
  return(sum2)
}
#함수 사용
n1=1
n2=10
getSum2(n1,n2) # 왜 에러??

# 예3) European Vanilla Call-Option에 대한 Black-Scholes 공식
getBSCallPrice = function(T, K, S, sig, r)
{
  #T=옵션 만기, K=옵션 행사가격, S=기초자산 가격, sig = 변동성, r = 이자율
  d1 = (log(S/K) + (r+0.5*sig^2)*T)/(sig*sqrt(T))
  d2 = d1 - sig*sqrt(T)
  BSCall = S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
  return(BSCall)
}
#함수 사용
T=1; K=95; S=100; sig=0.2; r=0.02
getBSCallPrice(T, K, S, sig, r)

