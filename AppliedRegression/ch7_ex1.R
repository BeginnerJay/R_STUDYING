## 로버스트 회귀 -> 이상치에 덜 민감 -> 데이터 작거나 이상치, 영향치 있을 때 유용

# 이상치 -> 모형 잘 따르지 않는 값
# 지렛점 -> 평균으로부터 멀리 떨어진 예측변수에서 측정된 관측값(회귀계수 추정에 영향)
# 영향점 -> 회귀식에 크게 영향을 미치는 관측값
# 쿡의 거리 -> i번째 관측값이 제거될 때, 회귀함수가 얼마나 크게 변하는지에 대한 척도

### 미국 51개 주의 범죄 관련 자료에 대해 로버스트 회귀 수행
library(foreign)
cdata <- read.dta("http://stats.idre.ucla.edu/stat/data/crime.dta")
str(cdata) # single: 한부모 비율
summary(cdata)

## OLS
summary(ols <- lm(crime ~ poverty + single, data = cdata))
# 회귀진단그림
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(ols, las = 1) # 9,25,51번 관측치가 잔차와 지레에서 큰 값 나온다. 쿡의 거리도 크다.
cdata[c(9,25,51), 1:2] # 플로리다, 미시시피, 워싱턴dc가 큰 지렛값(또는 큰 잔차)를 갖는다.

library(MASS) # 쿡의 거리가 기준값인 4/n 보다 큰 관측값 출력
d1 <- cooks.distance(ols)
r <- MASS::stdres(ols) # 표준화잔차값도 같이 출력력
a <- cbind(cdata, d1, r)
a[d1 > 4/51, ]
# 잔차의 절댓값이 큰 관측값 출력
rabs <- abs(r)
b <- cbind(cdata, d1, r, rabs)
assorted <- a[order(-rabs), ]
assorted[1:10, ]
## 로버스트 회귀 적합 : rlm{MASS} 사용
# IRLS 추정 시 Huber의 가중함수 사용(기본값)
# psi = 옵션은 가중함수를 지정하며, "psi.huber", "psi.hampel", "psi.bisquare"가 있다.
summary(rr.huber <- rlm(crime ~ poverty + single, data = cdata)) # poverty 변수는 유의하지 않다.

# 잔차가 큰 상위 15개 자료에 대해 Huber의 가중 함수를 사용한 경우 가중치 출력
hweights <- data.frame(state = cdata$state, resid = rr.huber$wresid, weight = rr.huber$w)
hweights2 <- hweights[order(rr.huber$w), ]
hweights2[1:15, ] # 회귀모수의 추정 시, 잔차가 큰 자료일수록 작은 가중치가 적용되고 있다.

## 로버스트 회귀적합 : Tukey의 bisquare 가중함수 사용
rr.bisquare <- rlm(crime ~ poverty + single, data = cdata, psi = psi.bisquare)
summary(rr.bisquare)
biweights <- data.frame(state = cdata$state,
                        resid = rr.bisquare$residuals,
                        weight = rr.bisquare$w)
biweights2 <- biweights[order(rr.bisquare$w), ]
biweights2[1:15, ] # 가중함수의 종류에 따라 가중치가 많이 달라지며, 따라서 회귀계수의 추정값도 달라짐.
# 가중함수의 종류에 따라 장단점이 있다
  # Huber의 가중함수는 심한 이상치를 가지는 경우 어려움이 있음
  # bisquare 가중함수는 수렴이 안 되거나 다중의 해를 가질 수 있다. 