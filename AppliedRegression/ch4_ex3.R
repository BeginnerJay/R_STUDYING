state <- as.data.frame(state.x77)
str(state)
colnames(state)[4] <- "Life.Exp"
colnames(state)[6] <- "HS.Grad"

state[, 9] <- (state$Population*1000)/state$Area # 인구밀도를 나타내는 변수 생성
colnames(state)[9] <- "Density"
str(state); summary(state); cor(state); pairs(state); cor.test(state)

model1 <- lm(Life.Exp ~ ., data = state)
summary(model1)
summary.aov(model1) # summarize AOV model

model2 <- update(model1, .~. -Area)
summary(model2)
anova(model1, model2) # 모형 간 비교
model3 <- update(model2, .~. -Illiteracy)
model4 <- update(model3, .~. -Income)
model5 <- update(model4, .~. -Density)
model6 <- update(model5, .~. -Population)
summary(model6)

# update(object, ...) 설명 시작
# ## Default S3 method:
# update(object, formula., ..., evaluate = TRUE)
# 
# getCall(x, ...)
# Arguments
# object, x	
# An existing fit from a model function such as lm, glm and many others.
# 
# formula.	
# Changes to the formula – see update.formula for details.
# 
# ...	
# Additional arguments to the call, or arguments with changed values. Use name = NULL to remove the argument name.
# 
# evaluate	
# If true evaluate the new call else return the call.
# 
# Value
# If evaluate = TRUE the fitted object, otherwise the updated call.

# Examples
oldcon <- options(contrasts = c("contr.treatment", "contr.poly"))
## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
## Page 9: Plant Weight Data.
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2, 10, 20, labels = c("Ctl", "Trt"))
weight <- c(ctl, trt)
lm.D9 <- lm(weight ~ group)
lm.D9
summary(lm.D90 <- update(lm.D9, . ~ . - 1))
options(contrasts = c("contr.helmert", "contr.poly"))
update(lm.D9)
getCall(lm.D90)  # "through the origin"

options(oldcon)
# update 설명 끝


model.step <- step(model1, direction = "backward")
summary(model.step) # 인구와 고졸비율이 높으면 기대수명 증가, 살인율과 연평균영하기온일수 높으면 기대수명 감소

confint(model.step) # 회귀계수에 대한 신뢰구간 구하는 함수
# 주어진 자료에 대한 예측값 구하기
predict(model.step, list(Population = 4000, Murder = 10.5, HS.Grad = 48, Frost = 100))
# 여러 가지 회귀진단의 결과
par(mfrow = c(2,2))
plot(model.step)
# 좌상단 그림 -> 잔차그림, 패턴 없음 ->적절
# 우상단 -> 정규확률그림 -> 직선상에 잘 위치 -> 잔차의 정규성 가정 만족
# 좌하단 -> 적합값에 대한 표준화잔차의 제곱근 -> 정규분포의 가정 만족
# 우하단 -> 지렛값에 대한 표준화잔차, 영향점 진단을 위한 쿡 거리 
# -> 지렛값 큰 점 몇 개 보임(영향점), 동시에 큰 잔차를 가지는 점 한 개 포함(이상치)

# 다중회귀모형의 적합결과 추출
names(model.step)
model.step[[1]]
model.step[[2]]
sort(model.step$residuals)

# 베타 계수(표준화게수) -> 모든 변수들이 표준화되었을 때의 회귀계수
# 예측 변수들의 상대적인 중요도를 비교할 때 베타 계수가 유용하다.
# 비표준화계수 또는 p-value만으로는 상대적인 중요도를 알 수 없다.
# 모든 계수에 scale()을 적용한 후 lm()을 수행한다.
model.beta <- lm(Life.Exp ~ scale(Population) + scale(Murder) + 
                    scale(HS.Grad) + scale(Frost), data = state)
summary(model.beta)
# 베타계수의 절대값을 비교해 보면, 변수의 중요도는 Murder > HS.Grad > Frost > Polulation 순이다.

# 편상관계수(partial)
  # 두 변수 간의 상관계수
  # 두 변수 각각에서(회귀를 통해) 제 3의 변수들의 효과를 제거한 후
  # 잔차들 간의 상관계수를 구한 것
# 부분상관계수(준편, semipartial)
  # 두 변수 간의 상관계수
  # 이 중 한 변수에게만 제 3의 변수들의 효과를 제거한 후
  # 잔차와의 상관계수를 구한 것
# ggm::pcor()이 편상관계수 제공

# analysis와 statistics가 주어질 때, vectors와 algebra간의 상관계수
# pcor(c("vectors", "algebra", "analysis", "statistics"), var(marks))
# pcor(c(2,3,4,5,), var(marks))와 동일