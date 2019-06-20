data(swiss); force(swiss); str(swiss)
# 모든 가능한 회귀 적용(1988년 스위스 출산, 경제)
install.packages("leaps"); library(leaps)
a <- regsubsets(x = Fertility ~ ., data = swiss, nbest = 3) # Fer를 반응변수, 나머지는 예측변수
summary(a)
par(mfrow = c(1,2))
plot(a, sacle = "adjr2"); plot(a, scale = "bic")

coef(a, 1:3)
vcov(a, 3)

summary(lm1 <- lm(Fertility ~ . , data = swiss))

# 자동화된 변수 선택법 이용
slm1 <- step(lm1, direction = "both", data = swiss)
summary(slm1)

slm1$anova
# 모형에 포함된 예측 변수들 간의 상대적인 중요도를 파악하기 위해
# 표준화된 예측변수를 사용한 결과를 제시한다.