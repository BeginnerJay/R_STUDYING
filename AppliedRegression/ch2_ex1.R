# iris 자료에서 임의로 결측값을 발생시킨 자료에 대해 대치 수행
# prodNA(missForest) 함수는 자료에서 인위적으로 결측값 발생시켜준다.
install.packages("missForest")
require(missForest)
set.seed(13123313)
iris
md <- missForest::prodNA(iris, noNA = 0.1)
head(md)

# mice::mice() 함수는 다변량 자료의 결측값에 대해 다중 대치를 제공한다.
# (Multiple Imputation by Chained Equations)
install.packages("mice")
library(mice)
mice::md.pattern(md) # 결측값의 패턴을 보여 줌

# VIM::aggr() 함수는 결측값의 패턴을 시각화하는데 유용하다.
install.packages("VIM")
library(VIM)
plot.1 <- VIM::aggr(md,
                    col = c('navyblue', 'yellow'),
                    numbers = TRUE,
                    sortVars = TRUE,
                    labels = names(md),
                    cex.axis = .7,
                    gap = 3,
                    ylab = c("Missing data", "Pattern"))

# mice::mice() 함수를 통해 결측값을 대치한다. method = 옵션은 다음과 같다.
  # method = "pmm", "logreg", "polyreg", "polr"
    # Predictive mean matching : 수치형 변수의 경우 기본값
    # Logistic Regression : 이진(수준이 2개) 변수에 대한 기본값
    # Bayesian Polytomous Regression : 요인(수준이 3개 이상)변수에 대한 기본값
    # Proportional Odds Model : 2개 이상의 순서형 변수에 대한 기본값

# https://m.blog.naver.com/tjdudwo93/220976082118
md.1 <- mice::mice(md,
                   m = 5, # 다중 대치의 수를 5개로 지정
                   maxit = 50, # 반복 횟수를 나타내는 스칼라값
                   method = 'pmm',
                   # 결측값을 포함하는 각 관측값에 대해, 해당 변수에 가장 가까운 예측 평균을 가진
                   # 관측값을 찾는다. 이 match로부터 관측된 값을 대치값으로 사용한다.
                   seed = 13123313) 

# 결측값 대치 후의 완비 자료 출력(5개 중 2번째 사용)
imputed.1 <- complete(md.1, 2)

# mice() 함수의 수행 결과 요약
summary(md.1)

# 다중 대치에 의한 5개(m = 5)의 데이터 셋에 대해 각각 특정 모형을 적합하고, 이를 결합하는 과정 시연
# 결과에서 fmi는 결측값의 비율, lambda는 결측값으로 인한 전체 분산의 비율 나타냄
options(digit = 3)
fit.1 <-with(md.1, lm(Sepal.Length ~ Sepal.Width))
pool.1 <- pool(fit)
summary(pool.1)

# missForest::missForest() 함수는 랜덤 포레스트 알고리즘을 사용하여 대치를 수행한다.
  # 이 방법은 비모수적 대치 -> 다양한 변수 형태에 적용 가능
  # 이 방법은 Out of Bag(OOB) 대치 오차 추정값을 제공해 준다.
md.2 <- missForest::missForest(md) # 기본값 옵션을 사용하여 결측값을 대치함.
names(md.2)
md.2$OOBerror
# NRMSE는 정규화된 MSE(평균제곱오차)로, 연속적인 값의 대치로부터 유도되는 오차를 나타낸다.
# PFC(proportion of falsely classified)는 범주형 값의 대치로부터 유도되는 오차를 나타낸다.

# 실제 자료와의 비교
error <- mixError(md.2$ximp, md, iris)
error 
# 연속형 변수는 18%의 오차로 대치되고, 범주형 변수는 8% 오차로 대치된다는 것을 나타낸다.
  # 이것은 mtry = 와 ntree = 옵션을 조정하여 향상시킬 수 있다.
    # mtry = : 각 분할에서 무작위로 샘플링되는 변수의 수
    # ntree = : 포레스트에서 생성할 트리의 수

# Hmisc::impute()와 Hmisc::aregImpute() 함수는 결측값 대치를 제공한다.
  # impute() : 사용자가 지정한 방법(평균, 중앙값, 최댓값, 최솟값)으로 결측값 대치
  # aregImpute() : 가법회귀, 부트스트랩, PMM을 사용한 평균 대치를 수행(변수 형태를 자동으로 식별)
install.packages("Hmisc")
library(Hmisc)
md.31 <- with(md, impute(Sepal.Length, mean))
md.32 <- with(md, impute(Sepal.Length, 'random'))

md.33 <- aregImpute(~ Sepal.Length + 
                      Sepal.Width + 
                      Petal.Length +
                      Petal.Width +
                      Species, data = md, n.impute = 5)
md.33 # R^2 값으로 예측 성능 평가