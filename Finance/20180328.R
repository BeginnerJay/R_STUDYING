x <- list(1,2,"3","good") # 리스트 만들기
y <- list(name = "chang", age = 10, title = "mananger") # 원소에 이름을 붙여서 리스트 만들기
z <- list(element.num = c(1,2,3), element.char = c("a","b"))
x          
y
z

x <- list(name = "chang", age = 10, score = c(70,90), grade = c("B","A"))
x[1]; x[[1]] #비교
typeof(x[1]); typeof(x[[1]]) #비교
x[2];x[[2]]
x[3];x[[3]]

x$name
typeof(x$name)
x$age
x$age[2] # list의 원소의 원소에 접근

# Array(배열) <- array(), dim(), dimnames() 등

# Data Frame(데이터 프레임)
## 생성 <- data.frame()
## 원소 조작 <- attach(), detach()

# 2. 추가적인 기본 사용법
## (1) 제어문
### 1) switch() 함수 <- switch(변수,변수값1={실행문1},변수값2={실행문2}, ~ , {실행문}) <- 변수값에 해당하는 실행문 실행. 일치하는 변수값이 없는 경우 마지막 실행문을 실행

# 최빈값(mode) 계산 함수
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}
getmode(x)
# 표본
x <- c(seq(1:10), seq(20:40), seq(80:100), 30)
x

# 대표값 구하기
hist(x, breaks = 30, probability = FALSE)
type <- 'mean'
switch(type,
       mea = {mean(x)}, # mean
       med = {median(x)}, # median
       mod = {getmode(x)}, # mode
       {mean(x)})

### 2) while문 <- while(Condition) {Condition이 TRUE일 때의 실행문} <- Condition이 TRUE인 동안 실행문을 반복해서 시행.

# f(x) = k 풀기: x n+1 = x n - [f(x n )-k]/f'(x n )
# 문제
f <- function(x) x^2 # 목적함수
k <- 5 # 목적값
# 알고리즘 수행을 위한 사전 세팅
f1 <- function(x) 2*x # 목적함수의 1차 미분
tol <- 1e-10 # 허용오차
x0 <- k/2 # 초기값
# 알고리즘 수행
x <- x0
iter <-0
repeat{
  if(abs(f(x)-k)<tol) break
  x <- x - (f(x)-k)/f1(x)
  iter <- iter + 1
}
x; x^2; iter

# Monte Carlo Simulation으로 원주율 계산하기
set.seed(1) # 매 시뮬레이션마다 동일한 난수가 생성되도록 seed number 고정.
l <- 2 # 정사각혀으이 한 변의 길
r <- l/2 # 반지름
M <- 10000 # 시뮬레이션 횟수
x <- runif(M, -r, r) # U(-r,r)
y <- runif(M, -r, r) # U(-r,r)
in.prob <- sum(sqrt(x^2+y^2)<=r)/M
app.pi <- in.prob*(l^2)
app.pi # 근사값
pi # 참값

