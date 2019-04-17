##### Chapter 7: Neural Networks and Support Vector Machines -------------------

# 활성 함수는 인공 뉴런이 들어오는 정보를 처리해서 네트워크를 통해 정보를 처리하는 메커니즘.
# (로지스틱)시그모이드 함수를 자주 쓴다.
# 선형 함수를 활성 함수로 쓰면 선형 회귀와 매우 유사한 신경망르 만든다.
# 가우시안 활성 함수는 방사 기저 함수 네트워크라고 하는 모델을 만든다.
# 이들 각각은 특정 학습 작업에 좀 더 적합한 강점을 가진다.
# 여러 활성 함수의 경우, 출력 신호에 영향을 미치는 입력 값의 범위가 상대적으로 좁다는 것을 인식해야 한다.
# ex) 시그모이드는 절대값이 5를 넘어가는 값은 거의 0이나 1이다.
# 압축 문제에 대한 해결책은, 특징 값이 0 근처의 작은 범위 안으로 들어오게 모든 신경망 입력을 변환하는 것이다.
# 이 과정에서 표준화나 정규화가 수반된다.

# 네트워크 토폴로지(네트워크 패턴, 네트워크 구조)
# 계층 갯수, 네트워크의 정보가 역방향으로 이동할 수 있는지 여부, 네트워크의 각 계층 별 노드 개수로 구별.
# 다중 은닉 계층을 갖는 신경망을 DNN이라고 하며, 그런 신경망의 훈련을 딥러닝이라고 한다.

# 신경망은 각 계층의 노드 개수로 복잡도에 변화를 줄 수 있다.
# 입력 노드 개수는 입력 데이터의 특징 개수로 사전에 결정된다.
# 비슷하게 출력 노드의 갯수는 모델링되는 출력 개수 또는 출력 클래스 레벨 개수로 사전에 결정된다.
# 하지만 은닉 노드의 갯수는 모델을 훈련하기 전에 사용자가 결정한다.
# 은닉 계층의 뉴런 갯수를 결정하는 신뢰할 많나 규칙은 없다.
# 적합한 갯수는 다양한 요소 중, 입력 노드 수, 훈련 데이터양, 잡음 데이터양, 학습 작업의 복잡도에 따라 달라진다.
# 뉴런 갯수가 아주 많으면 오버피팅 가능성!
# 대부분의 경우 신경망은 적은 갯수의 은닉 노드 만으로도 엄청난 학습 능력 제공한다.

# 네트워크는 경험으로 훈련된다. 신경망이 입력 데이터를 처리하면서 뉴런 사이에 연결은 강화되거나 약화된다.
# 네트워크의 연결 가중치는 시간이 지나면서 관측되는 패턴을 반영하도록 조정된다.
# 순방향 단계
  # 입력 계층부터 출력 계층까지 뉴런이 순차적으로 활성화되면서
  # 도중에 뉴런의 가중치와 활성 함수가 적용된다.
  # 마지막 계층에 도달하면 출력 신호가 생성된다.
# 역방향 단계
  # 순방향 단계에서 만들어진 네트워크 출력 신호를 훈련 데이터의 실제 목표 값과 비교한다.
  # 네트워크 출력 신호와 실제 값의 차로 오차가 만들어지면,
  # 네트워크에서 역방향으로 전파되어 뉴런 사이에 연결 가중치를 수정하고, 미래의 오차를 줄인다.
# 뉴런의 입력과 출력 사이의 관계가 복잡한데, 알고리즘이 가중치를 얼마나 바꿔야 하는지 어떻게 결정하는가?
  # 경사 하강법!
    # 역전파 알고리즘은 각 뉴런의 활성 함수를 미분해 들어오는 각 가중치 방향의 그래디언트를 식별한다.
      # 그래서 미분 가능한 활성 함수를 갖는 것이 중요하다.
    # 그래디언트는 가중치의 변화에 대해 오차의 감소 또는 증가 방향(경사)를 나타낸다.
    # 알고리즘은 오차가 최대한 감소하도록, "학습률만큼" 변경하려고 할 것이다.
    # 학습률이 커질수록 알고리즘은 더 빠르게 언덕을 내려온다.->훈련 시간을 줄일 수 있다.


##### Part 1: Neural Networks -------------------
## Example: Modeling the Strength of Concrete  ----

## Step 2: Exploring and preparing the data ----
# read in data and examine structure
getwd()
setwd("D:/R_LAB/MLwR/Chapter 07")
concrete <- read.csv("concrete.csv")
str(concrete)

# custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}
# apply normalization to entire data frame
concrete_norm <- as.data.frame(lapply(concrete, normalize))
# confirm that the range is now between zero and one
summary(concrete_norm$strength)
# compared to the original minimum and maximum
summary(concrete$strength)
# 모델을 훈련하기 전에 데이터에 적용한 모든 변환은 이후에 역으로 적용해서 원래 측정 단위로 되돌려야 한다.
# 재조정을 용이하게 하려면 원래 데이터나, 원래 데이터의 요약 통계를 저장해 두는 것이 좋다.


# create training and test data
# 이미 임의로 정렬되어 있으므로, 데이터 프레임을 단순히 두 부분으로 나누면 된다.
concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]

## Step 3: Training a model on the data ----
# train the neuralnet model
install.packages("neuralnet")
library(neuralnet) # 사용하기 쉬운 표준 신경망의 구현을 제공 & 네트워크 토폴로지 그리는 함수 제공.

# simple ANN with only a single hidden neuron(은닉 노드 하나인 다층 순방향 네트워)
set.seed(13123313) # to guarantee repeatable results
concrete_model <- neuralnet(formula = strength ~ cement + slag +
                              ash + water + superplastic + 
                              coarseagg + fineagg + age,
                              data = concrete_train)

# visualize the network topology
plot(concrete_model)
# 은닉 노드가 하나인 신경망은, 선형 회귀 모델과 비슷할 수 있다.
  # 각 입력 노드와 은닉 노드의 가중치는 회귀 계수와 비슷하다.
  # 바이어스 항의 가중치는 절편과 비슷하다.
# 아래 부분에 훈련 단계 횟수와 오차 제곱 합을 보고한다.

## Step 4: Evaluating model performance ----
# obtain model results
model_results <- compute(concrete_model, concrete_test[1:8])
# compute()는 predict()와는 다르게 작동한다.
  # 이 함수는 두 개의 구성 요소로 된 리스트를 반환한다.
    # $neurons는 네트워크의 계층별로 뉴런을 저장하며,
    # $net.result는 에측 값을 저장한다.

# obtain predicted strength values
predicted_strength <- model_results$net.result
# 지금은 분류 문제가 아닌 수치 예측 문제이기 때문에, 모델의 정확도를 검토할 때에 혼동 행렬 사용 불가.
# 대신 예측된 콘트리트 강도와 실제 값의 상관관계를 측정해야 한다.

# examine the correlation between predicted and actual values
cor(predicted_strength, concrete_test$strength)

## Step 5: Improving model performance ----
# a more complex neural network topology with 5 hidden neurons
set.seed(13123313) # to guarantee repeatable results
concrete_model2 <- neuralnet(strength ~ cement + slag +
                               ash + water + superplastic + 
                               coarseagg + fineagg + age,
                               data = concrete_train, hidden = 5)

# plot the network
plot(concrete_model2)

# evaluate the results as we did before
model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength)

##### Part 2: Support Vector Machines -------------------
# 다차원 공간에 표시되는 점들 사이에 경계를 만드는 표면.
# 분류, 수치 예측을 포함한 거의 도든 유형의 학습 작업에 이용 가능.
# SVM은 점이 선형적으로 분리되지 않는 문제에도 확장될 수 있다.
# 서포트 벡터란 각 클래스에서 최대 마진 초평면(MMH)에 가장 가까운 점들이다.
  # 서포트 벡터만을 이용해서 MMH를 정의할 수 있다.
  # 특징의 개수가 엄청나게 많더라도, 서포트 벡터는 분류 모델을 저장하기 위한 아주 간결한 방법을 제공한다.
  # 서포트 벡터를 식별하는 알고리즘은 벡터 기하학에 의존한다. 하지만 과정의 기본 원리는 간단하다.
# 선형적으로 분리가 불가능한 경우
  # 슬랙 항을 추가해 비선형 데이터에 대해 훈련
  # 커널 트릭을 이용해 문제를 고차원 공간으로 매핑. 그렇게 하면 비선형적 관계가 선형적 관계로 바뀐다.
    # 커널 트릭은 측정된 특성 간의 수학적 관계를 표현하는 새로운 특징의 구성 과정을 포함한다.
# 장단점
  # 장점
    # 분류 또는 수치 예측 문제에 사용 가능
    # 잡음에 거의 영향을 받지 않음. 과적합도 쉽게 일어나지 않음.
    # 신경망을 사용하는 것보다 쉬움. 잘 지원되는 SVM 알고리즘들이 이미 존재
    # 정확도가 높은 편
  # 단점
    # 최고의 모델을 찾기 위해 커널과 모델 파라미터의 다양한 조합을 테스트해야 한다.
    # 훈련이 느릴 수 있으며, 특히 입력 데이터셋이 아주 많은 특징이나 예시를 갖는 경우 느리다.
    # 해석하기 어려울 수 있다.
# 커널
  # 일반적인 커널 함수는 특징 벡터 xi,xj에 변환을 적용하고, 내적으로 둘을 결합한다.
    # 이 때 내적은 두 벡터를 받아 하나의 숫자를 반환한다.
  # 선형 커널 : 데이터를 전혀 변환하지 않는다 -> 특징의 내적으로 간단히 표현 가능.
  # d차원 다항 커널 : 간단한 데이터의 비선형 변환을 더한다.
  # 시그모이드 커널 : 시그모이드 활성 함수를 사용한 신경망과 다소 유사한 SVM 모델을 만든다.
  # 가우시안 RBF 커널 : RBF 신경망과 유사하다. 많은 유형의 데이터에서 잘 작동한다.
# 커널을 특정 학습 작업에 연결해주는 신뢰할 만한 규칙은 없다.
  # 커널의 적합 여부는 훈련 데이터의 양, 특징, 그 관계, 그리고 학습될 개념에 크게 의존한다.
# 검증 데이터셋에 대해 몇 개의 SVM을 훈련하고 평가하는 식의 시행착오가 필요하다.

## Example: Optical Character Recognition ----

## Step 2: Exploring and preparing the data ----
# read in data and examine structure
letters <- read.csv("letterdata.csv")
str(letters)
# SVM 학습자는 모든 특징이 수치여야 하고, 게다가 각 특징이 아주 작은 구간으로 값이 조정되어야 한다.
# str을 해 보니 데이터의 정규화 또는 표준화가 필요하다는 것을 알 수 있다.
  # 모델을 적합시키기 위해 사용할 R 패키지가 자동으로 재조정을 실행한다.


# divide into training and test data
# 데이터는 이미 무작위로 나뉘어져 있다.
letters_train <- letters[1:16000, ]
letters_test  <- letters[16001:20000, ]

## Step 3: Training a model on the data ----
# begin by training a simple linear SVM
# 뛰어난 패키지들이 많다.
  # e1071 패키지는 LIBSVM 라이브러리의 R 인터페이스를 제공한다.(C++로 작성됨)
  # klaR 패키지는 SVM 구현을 R에서 직접 수행하는 함수를 제공한다.
  # kernlab  패키지는 caret 패키지와 함께 사용할 수 있어서, 다양한 자동화 방법(11장)으로 훈련 및 평가 가능.

install.packages("kernlab")
library(kernlab)
# ksvm() 함수는 디폴트로 가우시안 RBF 커널을 사용하지만, 다양한 옵션도 제공한다.
# SVM의 성능 측정 기준선을 마련하기 위해, 단순한 선형 SVM 분류기의 훈련으로 시작해보자.
letter_classifier <- ksvm(letter ~ ., data = letters_train,
                          kernel = "vanilladot") # vanilladot 옵션으로 선형 커널을 명시.

# look at basic information about the model
# 명령어를 동작시켜도, 실제 얼마나 잘 실행될 것인지에 대해서는 알려주지 않는다.
letter_classifier
# 모델이 범용 기능을 가지는지 알려면, 테스트 데이터 셋에 대한 성능을 검토해보자.

## Step 4: Evaluating model performance ----
# predictions on testing dataset 함수 predict() 이용
letter_predictions <- predict(letter_classifier, letters_test)
# type 파라미터를 지정하지 않았기 때문에, type ="response" 디폴트가 사용되었다. 

head(letter_predictions)
# 예측된 문자와 테스트 데이터셋에 있는 실제 문자 비교
table(letter_predictions, letters_test$letter)

# look only at agreement vs. non-agreement(전체적인 정확도만 평가)
# construct a vector of TRUE/FALSE indicating correct/incorrect predictions
agreement <- letter_predictions == letters_test$letter
table(agreement)
prop.table(table(agreement)) # 정확도 약 84%

## Step 5: Improving model performance ----
# 선형 커널 함수보다 더 복잡한 커널 함수를 사용해, 데이터를 더 높은 차원으로 매핑하여 더 나은 모델 적합 얻기 가능.
set.seed(13123313)

letter_classifier_rbf <- ksvm(letter ~ ., data = letters_train, kernel = "rbfdot")
letter_predictions_rbf <- predict(letter_classifier_rbf, letters_test)

agreement_rbf <- letter_predictions_rbf == letters_test$letter
table(agreement_rbf)
prop.table(table(agreement_rbf)) # 정확도 93%로 증가
