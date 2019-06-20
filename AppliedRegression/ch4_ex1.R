library(car)
data(Prestige)
str(Prestige); head(Prestige); summary(Prestige)
plot(Prestige[,1:4], pch = 15, col = "#FF44AA")
library(psych)
pairs.panels(Prestige[,1:4], scale = T)
pairs.panels(Prestige[,1:4], bg = c("#FFFFFF", "#FF44AA", "#FAC335")[Prestige$type], pch = 21)

# 다중선형회귀 적합 : lm()
mod1 <- lm(income ~ education + prestige + women, data = Prestige)
summary(mod1)
# education 변수가 유의하지 않음 -> 제거
mod2 <- lm(income ~ prestige + women, data = Prestige)
summary(mod2)
plot(mod2, pch = 16, which = 1) # 잔차 그림 -> 오차에 대한 등분산성 가정 위배 -> 개선 필요

# 다중회귀 적합 결과의 시각화
install.packages("rgl"); library(rgl)
newdat <- expand.grid(prestige = seq(10,90,by = 5), women = seq(0,100,by = 5))
newdat$pp <- predict(mod2, newdata = newdat)
install.packages("scatterplot3d"); library(scatterplot3d)
with(Prestige[, 1:4], plot3d(prestige, women, income, col = "#FF44AA", size = 1, type = "s"))
with(newdat, surface3d(unique(prestige), unique(women), pp, alpha = 0.3, front = "line", back = "line"))

# 변수선택 - 모든 가능한 회귀 -> leaps::regsuvsets(), 자동화된 변수선택 -> step()