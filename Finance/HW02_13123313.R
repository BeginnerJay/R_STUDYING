#2. 4. 1 Vector
x <- c(1,2,5)
typeof(x)
x <- c(1,2,5,"six")
typeof(x)

x <- 1:5
length(x) # 벡터의 길이를 알아낼 때 사용
length(x) <-7 # 벡터의 길이를 설정할 때 사용
x
length(x) <-4
x

# recycling rule
c(1,2,4) + c(3,5,8,2,0)

x <- 1:5
y <- 5:1
z <- c(x,y)
w <- append(x,y)
z
w

x <- 1:4
y <- 9:12
z <- c(x,y)
z

# 2. 4. 2 벡터 관련 함수
data.x <- rep(3,5)
data.x

vec <- c(rep(2,4), 1:3, c(8,1,9))
vec

data.x <- seq(2,10,2)
data.x

weight.x <- c(10,20,30)
length(weight.x)

set.seed(1)
x <- rnorm(1:5)
y <- rnorm(1:5)
x
y
pmin(x,y)
min(x,y)

x <- 1:5
rev(x)

# 2. 4. 2 matrix
A <- matrix(c(2,4,3,1,5,7), nrow = 2, ncol = 3, byrow = T) # byrow 함수가 참이면 행 단위로 채운다. 1행 다 채우고 2행 채우는 식
A

M <- matrix(1:6, nrow = 2)

M <- matrix(1:6, ncol = 3)

M

M[2,3]
M[ ,3]
M[2, ]
M[1, 1:2]

x <- matrix(1:10, nrow = 2, ncol = 5)
x[2,3]
x[2, ]
x[c(1,2), c(1,2,3)]
x[-c(1), ]

rownames(x) <- c("가","나")
colnames(x) <- c("갑","을","병","정","무")

# 2. 4. 3 행렬 관련 함수
m <- matrix(1,2,3)
m
dim(m)
ncol(m)

x <- matrix(1:4, nrow=2)
x
t(x) # 전치행렬

tmp <- matrix(0,2,2) # 모든 성분이 0
tmp

tmp <- matrix(,2,2) # 모든 성분이 NA
tmp

x<- matrix(1:4, nrow = 2)
y<- matrix(-1:2, nrow = 2)
x+y

x<- matrix(1:4, nrow = 2)
y<- matrix(-1:2, nrow = 2)
x%*%y # 행렬의 곱셈(그냥 *은 원소끼리 곱연산)

a <- 1:3
b <- 3:5
a %*% b # 벡터의 내적

I <- diag(5) # 항등행렬

I

x <- matrix(1:4,nrow = 2)
det(x) # 행렬식

c <- rbind(c(1,-1/4),c(-1/4,1))
c
solve(c) # 역행렬 구하기

B <- matrix(c(2,4,3,1,5,7),nrow = 3,ncol = 2, byrow = F)
C <- c(7,4,2)
cbind(B,C)
D <- t(C)
cbind(B,D)
F <- t(c(3,2))
rbind(B,F)

A<- matrix(c(2,4,3,1,5,7),nrow = 2,ncol = 3, byrow = T)
A
c(A)

# 2. 4. 4 데이터 다루기
x <- 1:5
x[3]
x[-2] # 해당 원소만 제외한 결과
x[c(1,3,5)]
x[2:4]

# (1) 인덱스 번호를 이용한 데이터 추출
x <- 1:10
x[1]
x[c(TRUE,FALSE)]
x[c(FALSE,TRUE)]
x[x%%3==0]
x[!(x%%3==0)]
x[-c(2,7)]

x <- c(1,2,3)
x[-1]
x[-1:-2] # 1~2원소 제외

x1 <-1:8
boolx <- c(T,T,T,F,T,T,F,F) #진리값 벡터를 이용한 원소 추출
x2<-x1[boolx]
x2

# (2) 논리연산자를 이용한 데이터 추출
set.seed(1) # 매번 생성되는 난수를 고정시키기 위해 set.seed 함수 사용
x <- rnorm(5)
x>0
x

nmb <- sum(x>0)
nmb

idx <- x>0
z <- x[idx]
z

idx2 <- which(x>0)
w <- x[idx2]
w

idx3 <- (x>0 & x<1)
x[idx3]

set.seed(1)
x <- rnorm(5)
x
which.max(x) # 원소 중 가장 큰 값이 몇 번ㅉㅐ에 있는지
which.min(x)

x <- c(0.1, 0.2, 0.7, 0.9)
which.max(x > 0.65) # 특정 조건을 만족하는 첫 원소

which.min(x>0.65)

v <- c('a', 'b', 'c', 'd')
'b'%in%v # 해당 원소가 있으면 TRUE, 없으면 FALSE 반환
match('b',v) # 조건을 만족하는 첫 원소의 번호 반환

x <- sample(1:10)
x
match(c(4,8),x)

sample(c('H','T'),5,replace = T) # sample(vec, n, replace=Boolean) <- vec에 지정된 숫자들을 n만큼 임의로 추출. replace함수가 T이면 복원, F면 비복원

x <- sample(1:4, 10, replace = T)
x
which(x %in% c(2,4)) # 여러 인덱스를 찾으려는 경우

x <- 1:5
x
ifelse(x%%2, "ev", "od") # ifelse(test, yes, no) <- test(벡터) 진술이 T면 yes수행, F면 no수행

x <- c(1,2,3)
View(x)

a <- 1:60
b <- a[seq(1, length(a), 6)] # 60개의 원소 중 매 6번째 원소를 추출하는 코드
print(b)

## 2.5 함수
# 2. 5. 1 사용자 정의 함수
# 함수명 <- function(인수1, 인수2,...){
#           본문
#         return(결과)
#}

# 객체에 값을 저장하기 위해서는 <-, ->, = 연산자 사용 ㄱㅏ능하지만, 함수에서 인자를 넣을 때에는 반드시 = 써여ㅑ 함.
# return이 없다면 함수에서 가장 마지막 문장의 결과값 반환
# 함수를 호출할 때에는 인자의 위치를 맞춰서 입력해 주어도 되고, 인자의 이름을 지정해서 입력해도 된다.

f <- function(x,y){x+y*2}
f(1,2) # 순서에 따라 인수를 입력한 경우
f(y = 2, x = 1) # 인수의 이름을 지정해서 입력한 경우

# '...' 인수는 함수 내부의 다른 함수의 인수를 표현하는 데 쓴다.

f <- function(x,y){x+y*2}
g <- function(z,...){
  + print(z)
  + f(...)
}
g(1,2,3)

f <- function() {return(list(first = 1, second = 2))} # 두 배열을 반환하는 함수는 list변수를 이용하여 구현할 수 있따.
# 함수 f는 결과값으로 1과 2를 반환하고자 한다. 이때 list 함수를 쓰고 그 안에원하는 변수 이름을 할당한 뒤, 1과 2를 지정해주자.
r <- f() # f를 수행한 결과를 객체 r에 담으면
r$first # 첫 번째 결과는 r$first라는 변수로 볼 수 있고
r$second # 두 번째 결과는 r$second라는 변수로 출력해 볼 수 있다.

# 값에 의한 전달 : 함수는 기본적으로 call by value를 사용하며, 특별한 방법을 사용하면 참조에 의한 호출(call by reference)를 구현할 ㅅ ㅜ있다. 따라서 특별히 지정하지 않으면 함수가 객체를 호출할 때 일반적으로 값만 전달된다(즉 객체가 복하되어 ㅐ해당 함수로 전달된다). call by reference는 공식지원x -> R을 잘 알아야함

# 2. 5. 2 함수와 변수의 범위
# 범위(Scope) : 코드에 기술한 변수 등을 지칭하는 이름이 어디에서 사용가능한지를 정하는 규칙
# 변수는 Scope에 따라 크게 지역변수와 전역변수로 나뉜다.

x <- 1
f <- function(){print(x)}
f() # 함수 f에서는 아무런 연산이나 변수할당이 없었으메옫 1이라는 값 출력 -> 이는 함수 밖에서 할당된 x변수가 함수 안에서도 사용되기 때문.

# 동일한 이름을 가진 함수가 함수 밖에서도 할당되고 안에서도 할당되면 혼동 줄 수 있음.

x <-1
f <- function(){x <-2 # 1줄과 2줄의 x는 이름만 같고 서로 다른 변수(like x')
+ print(x)}
x
f() # 함수가 바꾼 값은 내부의 x
x # 혼란

x<-1
f <- function(){x<<-2 # 앞의 결과와 달리 함수 내에서"<<-"로 변수를 재선언하게 되면, 함수 밖에서도 값이 변화한다.
+ print(x)}
x
f()
x

f <- function(x){print(y+w)}
source("test.R")
w <- 100
f(100)
