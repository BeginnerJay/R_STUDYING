##### Chapter 5: Classification using Decision Trees and Rules -------------------

# 트리에 다수의 명목 특징이 여러 레벨로 이루어져 있거나, 다수의 수치 특성이 있는 경우 tree overfit
# 매개 변수(parameter) 조정으로 이러한 단점을 극복할 수 있다.
# 트리 생성을 일찍 중단하거나(사전 가지치기 pre-prunning)
# 트리를 만든 후 데이터 포인터가 적은 노드를 삭제하거나 병합하는 것(사후 가지치기 post-prunning)

# 랜덤 포레스트와, 그래디언트 부스팅으로도 이러한 단점을 극복할 수 있다.

# 트리의 C5.0 알고리즘은 트리 식별 방법으로 엔트로피를 이용한다.
# 클래스가 n개인 경우 엔트로피의 범위는 0에서 log2(밑이 2)n이다.(클래스 값의 집합 내의 무작위성 또는 무질서를 정량화)

#### Part 1: Decision Trees -------------------

## Understanding Decision Trees ----
# calculate entropy of a two-class segment
-0.60 * log2(0.60) - 0.40 * log2(0.40) # 하나는 확률 0.6, 나머진 0.4인 경우

curve(-x * log2(x) - (1 - x) * log2(1 - x), # 클래스 2개일 때 모든 가능한 엔트로피 배열
      col = "red", xlab = "x", ylab = "Entropy", lwd = 4)
# 분할을 하기 위한 최적의 특징을 결정하기 위해 엔트로피를 사용하려면
# 알고리즘은, 각 특징별로, 분할로 인해 생기는 동질성의 변화를 계산한다.
# 이것이 정보 획득량이라고 하는 척도이다.
## 어떠한 특징 F의 정보 획득량은 분할 전 세그먼트 S1과 분할로 생성된 파티션들 S2의 엔트로피 차로 계산된다.
# 그런데 문제는 분할 후에 데이터가 하나 이상의 파티션으로 나뉜다
# -> S2 계산하는 함수는 모든 파티션의 전체 엔트로피 고려
# -> n개 파티션에 대해, 각 파티션의 엔트로피에 파티션이 속하는 예시 비율로 가중치 부여하여 합산.

# 정보 획득량 높음 -> 이 특징에 분할한 후 동질적 그룹을 생성하기 더 좋은 특징.
# 수치 특성에 대해 분할할 때에도 정보 획득량 사용 -> 어떠한 임계값보다 큰지 작은지로 나눈다. 그 임계값은 다양하게 테스트한다.
# 이러한 방식으로 수치적 특성을 2레벨 범주형 특성으로 나눈다.
# 그리고 분할 시 최대 정보 획득량을 산출하는 수치 절단점을 선택한다.

# 사전 가지치기를 하면, 감지하기는 힘들지만 트리가 크게 자랐다면 학습할 수도 있는 중요한 패턴을 놓칠 수도 있다.
# 그 대안으로 사후 가지치기를 한다.
# -> 먼저 overfit되는 큰 트리로 자라게 한 후, 나중에 분류 오류에 영향이 거의 없는 노드와 분기는 제거한다.
# -> 어떤 때에는 전체 분기가 위쪽으로 옮겨지거나, 좀 더 간단한 의사결정으로 대체된다.

# overfit과 underfit의 균형을 맞추는 것은 다소 기술적.
# 하지만 모델의 정확도가 필수적이라면 다양한 가지치기 옵션으로 테스트 데이터에 대해 성능 향상이 되는지 투자해야!       

## Example: Identifying Risky Bank Loans ----
## Step 2: Exploring and preparing the data ----
getwd()
setwd('D:/R_LAB/MLwR/Chapter 05')

credit <- read.csv("credit.csv")
str(credit)

# look at two characteristics of the applicant
table(credit$checking_balance)
table(credit$savings_balance)

# look at two characteristics of the loan
summary(credit$months_loan_duration)
summary(credit$amount)

# look at the class variable
table(credit$default)

# create a random sample for training and test data
# use set.seed to use the same random number sequence as the tutorial
# 훈련에 데이터 90% 사용, 테스트에 10% 사용
# 임의로 정렬되어 있지 않은 데이터를 다루는 방법 -> 랜덤 샘플
set.seed(123) # 랜덤 샘플 수행하기 전에 시드값 설정
train_sample <- sample(1000, 900)

str(train_sample)

# split the data frames
credit_train <- credit[train_sample, ]
credit_test  <- credit[-train_sample, ]

# check the proportion of class variable
# 잘 나뉘었다면 각 데이터셋은 약 30%의 채무불이행 대출을 갖는다.
prop.table(table(credit_train$default)) # 잘 나뉘었다
prop.table(table(credit_test$default)) # 잘 나뉘었다

## Step 3: Training a model on the data ----
# build the simplest decision tree
if(!require(C50)) {
  install.packages("C50")
  require(C50)
}
# ?C5.0Control 명령으로 알고리즘을 정교하게 조정하는 방법에 대한 더 자세한 사항을 확인할 것

# credit_train의 17번째 열은 default 클래스 변수이므로 제외한다.
# 그리고 분류를 하기 위한 목표 팩터 벡터로 제공해야 한다.
credit_model <- C5.0(credit_train[-17], credit_train$default)

# display simple facts about the tree
credit_model # 트리 크기 57 -> 트리가 57개의 결정으로 이뤄진 깊이라는 뜻.

# display detailed information about the tree
summary(credit_model)
# 가끔 트리는 논리적으로 이해가 안 되는 결정을 만든다.
# ex) 왜 대출 이력이 좋은 신청자가 채무불이행 가능성 있지? 왜 수표 계좌 잔고를 모르면 채무불이행 가능성 없지?
# 그런 규칙들은 데이터의 실제 패턴을 반영한 결과일 수도 있고
# 단지 통계적 이상치일 수도 있다.
# 두 경우 모두, 트리의 논리가 비즈니스 용도로 합당한지 확인하기 위해
# 이런 이상한 결정을 조사해보는 것이 중요하다.

# 트리를 출력한 후 혼동 행렬을 보여준다.
# 혼동 행렬은 모델이 훈련 데이터에서 부정확하게 분류한 레코드를 보여주는 교차표다.

## Step 4: Evaluating model performance ----
# create a factor vector of predictions on test data
credit_pred <- predict(credit_model, credit_test)

# cross tabulation of predicted versus actual classes
library(gmodels)
# gmodels 패키지에 있는 CrossTable() 함수를 통해, 이 벡터와 실제 클래스 값을 비교할 수 있다.
CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
# prop.c = FALSE, prop.r = FALSE 하면 표에서 열과 행 비율을 제거한다.

# 남은 백분율 prop.t는 전체 레코드 개수에서 셀의 레코드 비율을 나타낸다.


## Step 5: Improving model performance ----
# 실제 채무불이행 중 42%만 정확히 예측했다. 이는 매우 큰 비용이 될 수 있다.

## Boosting the accuracy of decision trees
# boosted decision tree with 10 trials(부스팅 팀에 사용할 독립적 트리의 갯수를 10개로 늘림)
credit_boost10 <- C5.0(credit_train[-17], credit_train$default,
                       trials = 10)
# rules = TRUE를 명시하면 분류 규칙을 이용해 모델을 생성하게 된다.



credit_boost10 # 트리의 크기가 57에서 47로 줄어들었다.
summary(credit_boost10)
# 부스팅을 추가하기 전 오류율보다 매우 줄어들었다.

# 테스트 데이터에서도 성능 향상이 될까?
credit_boost_pred10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
# 오류율이 27에서 18로 줄었다. 채무불이행은 여전히 예측율이 낮다.
# 더 크게 개선되지 않은 이유
  # 1. 상대적으로 작은 훈련 데이터셋과 함수 관계
  # 2. 이 문제 자체가 원래 해결하기 어려운 문제

# 부스팅을 쓰지 않는 이유 -> 계산량 증가, 훈련 데이터에 잡음이 많으면 부스팅으로 개선되지 않음.


## Making some mistakes more costly than others

# create dimensions for a cost matrix
# 트리가 좀 더 비용이 많이 드는 실수를 하지 못하도록
# 여러 오류 유형에 페널티를 줄 수 있음.
matrix_dimensions <- list(c("no", "yes"), c("no", "yes"))
names(matrix_dimensions) <- c("predicted", "actual")
matrix_dimensions

# build the matrix
# 행렬을 채울 때 열 단위로 위에서 아래로 채우기 때문에, 특정 순서대로 값을 제공해야 한다.
# pre x act x
# pre o act x
# pre x act o
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2, dimnames = matrix_dimensions)
error_cost

# apply the cost matrix to the tree
credit_cost <- C5.0(credit_train[-17], credit_train$default,
                          costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)

CrossTable(credit_test$default, credit_cost_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
# 거짓 긍정을 증가시킨 대가로 거짓 부정을 줄였다.
# 전체적인 실수는 증가했지만, 이렇게 하는 편이 오히려 이익이 될 수도 있다.

#### Part 2: Rule Learners -------------------

# 분류 규칙은, 클래시를 레이블이 없는 예시에 할당하는 논리적인 if-else문으로 지식을 표현한다.
# -> 이것이 발생하면, 저것이 발생한다.

# 규칙은 tree로도 생성할 수 있는데 왜 별도의 규칙 학습자 알고리즘에 신경을 쓰는가?
# -> 규칙 학습자가 규칙을 직접 찾음으로써 피할 수 있는 작업에 대해,
# 의사 결정 트리는 특정 편향을 불러오기 때문이다.

# 규칙 학습자는 일반적으로, 특징이 주로 또는 전체적으로 명목형인 문제에 적용된다.
# 희소한 사건이, 특징 값 사이에 매우 특정한 상호작용에서 발생한다 하더라도, 규칙 학습자는 희소 사건을 잘 식별함.

# 클래스 값을 식별하는 점점 더 구체적인 규칙을 생성해 데이터를 드릴 다운한다.
# 규칙이 데이터의 부분을 커버하는 것 같아 보이기 때문에, 분리 정복 알고리즘은 커버링 알고리즘이라고도 한다.

# ZeroR -> 아무 규칙도 학습하지 않는 규칙 학습자(ex. 셋 중에 하나 뽑기)
## 1R 알고리즘 -> 하나의 규칙을 선택하는 방식.
  # 각 특징에 대해 유사한 값으로 데이터를 그룹으로 분리한다.
  # 그 다음 알고리즘은 각 세그먼트에 대해 대다수 클래스를 예측한다.
  # 각 특징에 기반을 두는 규칙의 오류율을 계산하고, 최소 오류를 갖는 규칙을 단일 규칙으로 선정한다.
# 1R 알고리즘은 너무 느리고, 잡음이 섞이면 쉽게 부정확해진다.

# RIPPER 알고리즘으로 규칙 학습자를 만들자.
# RIPPER 알고리즘의 3단계
  # 1. 기르기
    # 규칙에 조건을 탐욕스럽게 추가하기 위해 분할 정복 기법 사용
    # 데이터의 부분집합을 완벽하게 분류하거나, 분할을 위한 속성이 없어질 때까지 진행
    # 정보 획득량 기준 사용 -> 규칙이 더 정교햐져도 엔트로피가 줄지 않으면, 그 규칙은 가지치기된다.
  #
# 탐욕스러운 학습자 -> 선입 선처리 기반으로 데이터 사용
  # -> 한 번에 하나의 파티션을 만들고자 함. 먼저 가장 동질적인 파티션을 찾고 난 후에 차선을 찾고
    # -> 모든 예시가 다 분류될 때까지 계속해서 찾는다.
  # 단점 : 특정 데이터셋에 최적이고, 가장 정확하고, 최소 개수로 된 규칙을 생성한다는 보장이 없음
    # -> 데이터의 한 부분집합에 대해 정확한 한 개의 규칙은 계속해서 찾아낼 수 있지만
    # -> 전체 데이터셋에 대해 더 나은 전반적인 정확도를 갖는 좀 더 섬세한 규칙들을 개발할 기회 잃게 됨.

# 트리와 규칙 모두 탐욕 학습 휴리스틱 사용. 차이는
  # 트리는 이전 결정의 이력에 의해 영원히 제한된다.
  # 규칙은 규칙의 모든 조건으로 커버되지 않는 어떤 예시든 다음 규칙에 의해 재정복 가능.

## Example: Identifying Poisonous Mushrooms ----
## Step 2: Exploring and preparing the data ---- 
mushrooms <- read.csv("mushrooms.csv", stringsAsFactors = TRUE)

# examine the structure of the data frame
str(mushrooms)

# drop the veil_type feature(범주인데 하나밖에 없음 ->쓸모도없다)
mushrooms$veil_type <- NULL

# examine the class distribution
table(mushrooms$type)

## Step 3: Training a model on the data ----
library(RWeka) # JAVA 설치 필요

# train OneR() on the data
mushroom_1R <- OneR(type ~ ., data = mushrooms) # 1R 만들어내는 함ㅅ
# 규칙 학습자가 type ~. 을 통해 모든 특징을 고려하도록 만듦


## Step 4: Evaluating model performance ----
mushroom_1R
summary(mushroom_1R)

## Step 5: Improving model performance ---- # RIPPER 규칙 사용
mushroom_JRip <- JRip(type ~ ., data = mushrooms)
mushroom_JRip # 전체 9개의 규칙(if-else문)을 학습함.
summary(mushroom_JRip)

# Rule Learner Using C5.0 Decision Trees (not in text)
library(C50)
mushroom_c5rules <- C5.0(type ~ odor + gill_size, data = mushrooms, rules = TRUE)
summary(mushroom_c5rules)
