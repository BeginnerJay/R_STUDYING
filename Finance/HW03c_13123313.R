### 2.6.1 IF ELSE
# 문법
if (조건문) {문장2} else {문장1}

f <- function(x){
  if(x>0){
    ans <- "call"
  } else {
    ans <- "put"
  }
  return(ans)
}

source("test.R")
f(10)

# 만약 if문에 벡터형이 오면, 첫 번째 원소만 평가된 결과가 나온다.
x <- c(1,20)
f(x)

# 따라서 벡터형으로 입력받아 벡터형으로 결과를 반환하고자 한다면 다음과 같이 ifelse 함수를 사용해야 한다.
f <- function(x,y){
  ans <- ifelse(x>y, x, y)
  return(ans)
}

source("HW03c_13123313.R")
x <- c(1,20)
y <- c(20,5)
f(x,y)

### 2. 6. 2 SWITCH <- 특정 변수의 값에 따라 실행문을 실행한다.
#문법
switch (object,
  변수값1 = 실행값1,
  case2 = {action2},
  {action3})

centre <- function(x, type) {
  switch(type,
         mean = mean(x),
         median = median(x),
         trimmed = mean(x, trim = 0.1)) # 데이터 상하위 극단치 10%를 제하고 평균값 구하기
}

source('test.R')
x <- runif(100) # 0과 1 사이에서 균등분포를 따르는 난수를 100개 생성
x
centre(x, "median")

### 2. 6. 3 FOR <- 변수가 주어진 범위에서 값을 바꿀 동안 실행문을 수행한다.
#문법
for (variable in vector범위) {
  실행문  
}

# 1에서 10까지 홀수의 개수를 구하는 코드
x <- 1:10
k <- 0
for (i in 1:length(x)){if(x[i]%%2!=0) k <- k + 1}
k

# 범위에 인덱스를 주지 않고 아래와 같이 직접 변수 벡터를 이용할 수도 있다.

x <- 1:10
k <- 0
for (n in x){
  if(n%%2==1)
    k <- k + 1
  }
k

# for문을 이용한 그림 그리기
x <- seq(0,1,0.05) # 0부터 1까지 0.05 간격으로
x
plot(x,x,ylab = "y", type = "l")
for (j in 2:18) {lines(x,x^j)}

### 2. 6. 4 WHILE <- 조건이 참인 동안 실행문을 반복해서 실행한다(if+for). 반복 회수가 사전에 정해지지 않을 때 사용
# 문법
while(조건문){
  실행문
}

# 정규분포 난수를 연속적으로 생성하는데, 생성된 난수가 2보다 크면 반복문을 종료한다.
set.seed(1)
x <- rnorm(1)
counter <- 1
while (x<2) {
  x <- rnorm(1)
  counter <- counter + 1
}
print(counter)

# Newton-Raphson 방법을 통한 최적화 문제(제곱근 찾기)
y <- c(5,7)
x <- y/2 # 초기값 설정(뭘 주든 상관無)
while(any(abs(x*x-y)>1e-10))x <- (x+y/x)/2 # (x^2-y)의 값이 미리 정한 오차수준(여기서는 10^(-10))보다 작으면 해를 찾은 것이므로 종료한다. any 함수는 벡터의 인수 중 최소한 하나라도 참이면 TRUE를 돌려주는 함수이다.
x # while문을 이용한 결과값이다.
x^2 # 그 결과값을 실제로 제곱해보면 원하는 결과를 얻는다.

# any는 원소 하나라도 차이면 TRUE 반환, all은 모든 원소가 참이어야 TRUE 반환
x <- 1:5
any(x>2)
all(x>2)

### 2. 6. 5 BREAK <- 현재 실행중인 반복문을 중단한다.
# 균등분포 난수를 임의로 생성하는데, 값이 0.9보다 큰 경우 중단하는 코드
set.seed(1)
i <- 1
while (TRUE) {
  x <- runif(1)
  if(x>0.9){
    print(i)
    break
  }
  i <- i+1
}

###2. 6. 6 메모리 사전할당
# R은 두 벡터를 결합하거나 원소 하나를 추가할 때 매번 새로운 벡터를 만든다. 메모리 영역을 할당하고 초기화하는 등 많은 작업을 하게 되어 속도가 느려질 수가 있다. 미리 충분한 크기의 벡터를 만들어 두고 인덱스 번호로 접근해 값을 할당/수정하면 속도를 높일 수 있다. 따라서 미리 길이가 정해진 벡터를 만들면 좋다.

# 메모리 사전 할당을 하지 않은 경우
x < c()
y <- system.time(for(i in 100000000){ # 100000개로는 시간에 차이가 없어서 1억개로 진행
  x[i] <- i
})
print(y)
source('test.R')

# 메모리 사전 할당을 하는 경우
x <- c(NA)
length(x) <- 100000000
y <- system.time(for(i in 100000000){ # 100000개로는 시간에 차이가 없어서 1억개로 진행
  x[i] <- i
})
print(y)