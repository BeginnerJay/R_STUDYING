library(readxl); library(car); library(psych); library(DMwR)
Raw0 <- read.csv("Rawdata_2019.csv")
str(Raw0)

KNN0 <- knnImputation(Raw0[, !names(Raw0) %in% "medv"])
anyNA(KNN0);
KNN0.1 <- as.data.frame(scale(KNN0[2:10])) 

Sca_temp <- as.data.frame(scale(KNN0[2:10]))
Sca0 <- cbind(Raw0$Country, Sca_temp) # 정규화

pairs.panels(Sca_temp)

SM1 <- lm(Settler1900 ~ MortalityFactor, data = Sca0)
summary(SM1)
SM2 <- lm(LogProtection ~ Settler1900, data = Sca0)
summary(SM2)
SM3 <- lm(LogPPP ~ LogProtection, data = Sca0)
summary(SM3)
SM4 <- lm(LogPPP ~ MortalityFactor, data = Sca0)
summary(SM4)

with(Sca0, plot(MortalityFactor, Settler1900))
abline(SM1)
with(Sca0, plot(Settler1900, LogProtection))
abline(SM2)
with(Sca0, plot(LogProtection, LogPPP))
abline(SM3)
with(Sca0, plot(MortalityFactor, LogPPP))
abline(SM4)

Model <- lm(LogPPP ~ . -`Raw0$Country`, data = Sca0)
Model.step <- step(Model, direction = "both")
confint(Model.step)
summary(Model.step)
summary.aov(Model.step)

par(mfrow = c(2,2))
plot(Model.step)
vif(Model.step)
