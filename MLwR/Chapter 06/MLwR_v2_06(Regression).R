# 분류 방법 을 넘어서, 수치 데이터 간의 관계를 추정하는 기법
# 값들의 관계를 완벽히 나타내는 단일 함수가 늘 존재하지는 않음 -> 오차 범위를 정량화 필요
# 통계적 가설 검정에도 사용 가능 -> 통계학으로
# 기본적인 선형 회귀 모델 : 단순 선형 회귀, 다중 선형 회귀
# 회귀는 다른 유형의 종속 변수와 분류 작업에도 사용 가능.
# 로지스틱 회귀 : 이진 범주형 결과를 모델링하는 데 사용한다.
# 푸아송 회귀 : 정수 도수 데이터를 모델링한다.
# 다항 로지스틱 회귀 : 범주형 결과를 모델링하며, 분류에 사용된다.
# 모든 회귀 방법에 같은 기본 원리가 적용된다.
# y = b0x0 + b1x1 + ... + bixi + e (다중 선형 회귀) -> Y = Xb + e(행렬 표기). b도 e도 이제 벡터다.
# 이제 목표는 Y의 예측값과 실제 값 사이의 오차 제곱 합을 최소화하는 회귀 계수 b를 푸는 것이다.
# -> b = (X^T*X)^-1 * X^T*Y. X^T는 행렬 X의 전치행렬. X^-1은 역행렬.
getwd()
setwd("D:/R_LAB/MLwR/Chapter 06")

insurance <- read.csv("insurance.csv", stringsAsFactors = TRUE)
str(insurance)
# 회귀 모델을 구축하기 전에 정규성을 자주 확인해 보는 것이 좋다.
# 선형 회귀는 종속 변수가 정규 분포를 따르도록 엄격하게 요구하지는 않지만, 정규성을 갖는다면 좀 더 잘 적합된다.


# summarize the charges variable
summary(insurance$expenses)
# histogram of insurance charges
hist(insurance$expenses)
# table of region
table(insurance$region)


# exploring relationships among features: correlation matrix
cor(insurance[c("age", "bmi", "children", "expenses")])
# visualing relationships among features: scatterplot matrix
pairs(insurance[c("age", "bmi", "children", "expenses")])
# more informative scatterplot matrix
install.packages("psych")
library(psych)
pairs.panels(insurance[c("age", "bmi", "children", "expenses")])
# 타원 : 상관관계 타원형. 상관관계의 강도를 시각화한다. 타원이 늘어질수록 상관관계는 강해진다.
# bmi ~ children처럼 거의 완벽하게 둥근 달걀 모양은 아주 약한 상관관계를 나타낸다.
# 곡선 : 뢰스 곡선. x축과 y축 변수 사이의 일반적인 관계를 나타낸다.
# ex) age ~ children -> 노인과 젊은이들이, 중년보다 의료보험에서 보장하는 자녀수가 더 적다는 뜻.


## Step 3: Training a model on the data ----
ins_model <- lm(expenses ~ age + children + bmi + sex + smoker + region,
                data = insurance)
ins_model <- lm(expenses ~ ., data = insurance) # this is equivalent to above
# see the estimated beta coefficients
ins_model
## Step 4: Evaluating model performance ----
# see more detail about the estimated beta coefficients
summary(ins_model)

## Step 5: Improving model performance ----

# add a higher-order "age" term : 비선형 관계 추가
# 선형 회귀에서 독립 변수와 종속 변수 간의 관계는 선형인 것으로 가정되지만, 반드시 그럴 필요는  없다.
# 비선형 관계를 설명하려면 높은 차수의 항을 회귀 모델에 추가해 모델을 다항식으로 취급한다.
insurance$age2 <- insurance$age^2

# add an indicator for BMI >= 30
# 수치 변수를 이진 변수로 전환
# 특징의 영향이 누적되지 않고, 특정한 임계치에 도달한 후에만 영향을 갖는다고 가정한다면 임계치 위는 1 밑은 0
# ifelse()로 바꾼다.
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)

# 상호 작용 추가
# 어떤 특징들이 종속 변수에 대해 결합된 영향을 미친다면 어떻게 할 것인가(interaction)
# 두 변수가 상호작용을 하는 것으로 의심되면, 모델에 변수들의 상호작용을 추가해 이 가설 테스트 가능.
# expenses ~ bmi30*smoker 같은 식으로 상호작용시킨다.
# expenses ~ bmi30 + smokeryes + bmi:smokeryes. 확장 형식에서 : 연산자는 bmi30:smokeryes가 두 변수의 상호작용이라는 것을 의미.
# 확장 형식은 상호작용뿐만 아니라 bmi30과 smoker 변수도 자동으로 포함했다는 점을 기억할 것!

# create final model
ins_model2 <- lm(expenses ~ age + age2 + children + bmi + sex +
                   bmi30*smoker + region, data = insurance)
summary(ins_model2)


#### Part 2: Regression Trees and Model Trees -------------------
# 수치 예측을 수행하는 트리는 대단히 흥미롭지만, 회귀 모델링의 대안으로 간과된다.
## 좀 더 일반적인 회귀 방법에 비해 장점
# 의사 결정 트리의 장점과 수치 데이터 모델링 능력 결합, 사용자가 미리 모델 명시 불필요
# 자동 특징 선택 사용 -> 아주 많은 개수의 특징 사용 가능
# 선형 회귀보다 일부 데이터 타입에 아주 잘 맞음(특징이 많거나, 특징과 결과 간 매우 복잡 및 비선형 관계 존재)
# 수치 데이터의 분포를 가정하지 않음.
# 모델을 해석하는 데에 통계 지식이 필요하지 않음
## 단점
# 훈련 데이터가 많이 필요
# 결과에 대한 개별 특징의 전체적인 순 영향을 알아내기 어렵다.
# 큰 트리는 회귀 모델보다 해석하기가 더 어려워질 수 있다.

# 


## Understanding regression trees and model trees ----
## Example: Calculating SDR ----
# set up the data
tee <- c(1, 1, 1, 2, 2, 3, 4, 5, 5, 6, 6, 7, 7, 7, 7)
at1 <- c(1, 1, 1, 2, 2, 3, 4, 5, 5)
at2 <- c(6, 6, 7, 7, 7, 7)
bt1 <- c(1, 1, 1, 2, 2, 3, 4)
bt2 <- c(5, 5, 6, 6, 7, 7, 7, 7)

# compute the SDR
sdr_a <- sd(tee) - (length(at1) / length(tee) * sd(at1) + length(at2) / length(tee) * sd(at2))
sdr_b <- sd(tee) - (length(bt1) / length(tee) * sd(bt1) + length(bt2) / length(tee) * sd(bt2))

# compare the SDR for each split
sdr_a
sdr_b

## Example: Estimating Wine Quality ----
## Step 2: Exploring and preparing the data ----
wine <- read.csv("whitewines.csv")
# examine the wine data
str(wine)
# the distribution of quality ratings
hist(wine$quality)
# summary statistics of the wine data
summary(wine)

# 훈련 세트, 테스트 세트로 나눈다.
wine_train <- wine[1:3750, ]
wine_test <- wine[3751:4898, ]

## Step 3: Training a model on the data ----
# regression tree using rpart
# 회귀 트리 모델을 수행하기 위해 거의 모든 의사 결정 트리의 구현이 사용될 수 있다.
# 하지만 rpart(재귀적 분할)패키지는 회귀 트리의 구현에 가장 충실하다.
install.packages("rpart")
library(rpart)
m.rpart <- rpart(quality ~ ., data = wine_train)

# get basic information about the tree
m.rpart

# get more detailed information about the tree
summary(m.rpart)

# use the rpart.plot package to create a visualization
install.packages("rpart.plot")
library(rpart.plot)

# a basic decision tree diagram
rpart.plot(m.rpart, digits = 3)

# a few adjustments to the diagram
# 다이어그램에 포함된 숫자의 자리 수를 조정하는 digit parameter 외에, 시각화의 다른 많은 면들 조정 가능!
rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)

## Step 4: Evaluate model performance ----

# generate predictions for the testing dataset
p.rpart <- predict(m.rpart, wine_test)

# compare the distribution of predicted values vs. actual values
# 예측이 실제 값보다 훨씬 좁은 범위에 있다! 문제!
summary(p.rpart)
summary(wine_test$quality)
# 모델이 최고와 최악의 와인을 정확하게 식별하지 못한다는 것을 의미한다. 1에서3사분위까지는 예측력이 좋다.

# compare the correlation
cor(p.rpart, wine_test$quality)

# function to calculate the mean absolute error
MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))  
}

# mean absolute error between predicted and actual values
# 모델의 예측과 실제 품질 점수간의 차가 약 0.59라는 것을 의미한다.
MAE(p.rpart, wine_test$quality)

# mean absolute error between actual values and mean value
mean(wine_train$quality) # result = 5.87
MAE(5.87, wine_test$quality)

## Step 5: Improving model performance ----
# 모델 트리는 잎 노드를 회귀 모델로 대체함으로써 회귀 트리를 개선한다는 것을 기억하자.
# 모델 트리는 잎 노드에서 예측에 하나의 값만 사용하는 회귀 트리보다 더 정확한 결과를 낸다.

# train a M5' Model Tree
library(RWeka)
m.m5p <- M5P(quality ~ ., data = wine_train)

# display the tree
# 노드가 수치 예측이 아닌 선형 모델(LM1, LM2)로 종료된다.
# LM1로 추정된 영향이 이 노드에 도달한 와인 샘플에만 적용된다는 것을 주목하는 것이 중요.
# 전체 36개의 선형 모델이 이 모델 트리에서 만들어졌고
# 각각은 고정 산도와 다른 10개 특징의 영향에 대한 다른 추정치를 갖는다.
m.m5p

# get a summary of the model's performance
summary(m.m5p)

# generate predictions for the model
p.m5p <- predict(m.m5p, wine_test)

# summary statistics about the predictions
summary(p.m5p)

# correlation between the predicted and true values
cor(p.m5p, wine_test$quality)

# mean absolute error of predicted and true values
# (uses a custom function defined above)
MAE(wine_test$quality, p.m5p)

