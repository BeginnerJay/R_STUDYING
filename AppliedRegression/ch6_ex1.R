# 다중선형회귀모형에서는 오차항에 대해 epsilon_i ~ i.i.d, N(0, sigma^2)를 가정한다.

# 가중최소제곱(WLS, Weighted Least Squares)
  # 오차가 무상관이나, 오차(또는 반응변수)의 분산이 서로 다른 경우에 유용.
  # 가중치 w_i는 일종의 벌점으로, 잔차가 큰 관측값에 큰 값을 부여

# 반복재가중 최소제곱추정(IRLS, iteratively reweighted least squares)
  # 분산이 알려져 있지 않으나, 소수의 모수를 통해 추정될 수 있는 경우에 추정될 수 있는 경우
    # ex) Var(epsilon_i) = r0 + r1x1

# strongx{faraway} 자료에 대해 WLS와 IRLS 방법으로 회귀계수를 추정하고, OLS와 비교한다.
install.packages("faraway")
library(faraway)
data("strongx")
force(strongx)
pairs(strongx)
## MLS 추정 결과
m.1 <- lm(crossx ~ energy, data = strongx, weights = sd^-2)
summary(m.1)

## IRLS 추정 결과
a <- rep(1, 10)
m.2 <- lm(crossx ~ energy, data = strongx, weights = a)
for (i in 1:20) {
  tem <- lm(m.2$res^2 ~ crossx, data = strongx)
  g0 <- summary(tem)$coefficients[1]
  g1 <- summary(tem)$coefficients[2]
  w <- abs(1/(g0+g1*strongx$crossx))
  m.2 <- lm(crossx ~ energy, data = strongx, weights = w)
}
summary(m.2)

## OLS 추정과의 비교
m <- lm(crossx ~ energy, data = strongx)
summary(m)

## 시각화를 통한 OLS, WLS, IRLS 추정 결과 비교
plot(strongx$energy, strongx$crossx, col = "blue")
abline(a = m$coefficients[1], b = m$coefficients[2], lty = 2)
abline(a = m.1$coefficients[1], b = m.1$coefficients[2])
abline(a = m.2$coefficients[1], b = m.2$coefficients[2], col = "red")
legend("topleft", col = c("black", "black", "red"), lty = c(2,1,1), legend = c("OLS", "WLS", "IRLS"))
