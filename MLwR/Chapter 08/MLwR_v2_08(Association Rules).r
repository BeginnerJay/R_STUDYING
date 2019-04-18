##### Chapter 8: Association Rules -------------------
# 장바구니 분석의 결과는 연관 규칙의 모음이다.
  # 연관 규칙 : 아이템 집합 사이에 관계에 존재하는 패턴
    # LHS : 규칙을 실행하기 위해 만족돼야 하는 조건
    # RHS : 그 조건을 만족했을 때 기대하는 결과
# 연관 규칙은 예측이 아니라, 대규모 DB에서 자율적인 지식의 발견을 위해 사용된다.
# 연관 구칙 학습자는 자율적이기 때문에, 알고리즘이 훈련될 필요가 없다.
  # 즉 데이터가 사전에 레이블될 필요가 없다.
  # 하지만 학습자에 대해 사용자가 직접 확인하는 것 외에는 규칙 학습자의 성능을 측정할 방법이 없다.

# 연관 규칙 학습을 위한 a priori 알고리즘 (아 프리오리, 선험적인)
  # 쇼핑몰에 물건 100개만 있어도 2^100개의 아이템 집합을 평가해야 함
  # 좀 더 똑똑한 학습 알고리즘은 아이템 집합을 전부 평가하지 않는다.
    # 대신 잠재적인 아이템 조합이 실제로 드물게 발견된다는 사실을 이용한다.
  # 아프리오리 알고리즘은 빈번한 아이템집합의 속성에 대해 단순한 사전적 믿음을 이용한다.리
    # 통계 척도인, 아이템 집합의 흥미도를 이용한다.
      # 이는 어떻게 측정할까? 또 학습될 규칙의 수를 줄이기 위해 아프리오리 속성과 어떻게 결합되는가?
# 장단점
  # 장점
    # 대규모 거래 데이터에 대해 작업 가능
    # 이해하기 쉬운 규칙 생성
    # 데이터 마이닝과 DB에서 예상치 못한 지식 발굴 가능
  # 단점
    # 작은 데이터셋에서는 유용하지 않음
    # 진정한 통찰과 상식을 분리하기 위한 노력이 필요
    # 랜덤 패턴에서 비논리적인 결론을 도출하기 쉽다.
# 규칙 흥미 측정 : 지지도와 신뢰도
  # 지지도와 신뢰도를 사용해, 보고되는 규칙의 갯수를 제한할 수 있다.
    # 확실하거나 상식적인 규칙만 식별될 떄까지 제한하는 것도 가능하다.
  # support(X) = count(X) / N  <- (아이템 집합 X 를 포함하는 거래 건수 / 데이터베이스의 거래 건수)
  # confidence(X -> Y) = support(X,Y)/support(X) (X와 Y를 모두 포함하는 아이템집합의 지지도/X만포함지지도)
    # 신뢰도는 아이템 또는 아이템집합 X의 존재가, 아이템 또는 아이템집합 Y의 존재를 유발하는 거래의 비율
    # confidence(X -> Y) != confidence(Y -> X)
# a priori 원칙을 이용한 규칙 집합의 구축
  # 빈번한 아이템집합의 모든 부분집합 또한 빈번해야 한다.
  # a priori algorithm은 이 논리를 사용해, 실제 평가하기 전에 잠재적인 연관 규칙을 제외시킨다.
  # 규칙을 생성하는 절차
    # 1. 최소 지지도 임계치를 만족하는 모든 아이템 집합을 식별한다.
    # 2. 이 아이템 집합에서 최소 신리도 임계치를 만족하는 아이템 집합으로 규칙을 생성한다.



## Example: Identifying Frequently-Purchased Groceries ----
## Step 2: Exploring and preparing the data ----

# load the grocery data into a sparse matrix
# 거래 데이터는 좀 더 자유로운 형태로 저장되어 있다.
  # 각 레코드는 고정된 개수의 특징 대신, 한 개부터 여러 개까지 쉼표로 분리된 임의의 개수의 아이템목록으로 이루어짐.
install.packages("arules")
library(arules)
getwd()
setwd("D:/R_LAB/MlwR/Chapter 08")
# 그냥 read.csv로 읽으면 나중에 문제가 생긴다
  # 첫 번째 줄이 네 개의 값을 가졌기 때문에 R은 생성해야 할 네 변수를 생성했다. -> 더 많이 사면 잘린다
  # 거래를 특정 아이템으로 채우는 위치 집합이 아닌,
  # 각 특정 아이템을 포함하거나 포함하지 않는 장바구니로 취급하는 데이터셋이 필요하다.
# 거래 데이터를 위한 희소 행렬(sparse matrix)생성
  # 희소 행렬은 의미있는 셀에 비해 0이 너무 많기 떄문에, 전체 행렬을 통째로 저장하는 것은 비효율적이다.(170만개)
  # 희소 행렬 구조를 만들기 위해 arules 패키지를 사용하자.
groceries <- read.transactions("groceries.csv", sep = ",")
summary(groceries)# 0이 아닌 값 2.6%
# look at the first five transactions
  # 희소 행렬의 내용을 보려면 inspect 함수를 벡터 연산과 결합해 사용.
inspect(groceries[1:5])

# examine the frequency of items
itemFrequency(groceries[, 1:3])

# plot the frequency of items(아이템 빈도 그래프)
itemFrequencyPlot(groceries, support = 0.1) # 최소 10% 지지도 갖는 8개 아이템
itemFrequencyPlot(groceries, topN = 20) # 지지도 가장 높은 20개

# a visualization of the sparse matrix for the first five transactions
  # 5행 169열의 행렬. 5건의 거래와 169개의 가능한 아이템을 나타낸다.
image(groceries[1:5])

# visualization of a random sample of 100 transactions
image(sample(groceries, 100)) # 거래 100건에 대한 희소 행렬 시각화

## Step 3: Training a model on the data ----
library(arules)

# default settings result in zero rules learned
# 실행하는 것 자체는 간단하지만, 합리적인 개수와 연관 규칙을 생성하는 지지도, 신뢰도 찾기는 시행착오 필요
  # 파라미터의 수준이 너무 높은 경우
    # 규칙을 찾지 못하거나 너무 포괄적인 규칙만을 찾음(숟가락 옆에 젓가락 놓아봐야 다 안다)
  # 파라미터의 수준이 너무 낮은 경우
    # 규칙이 너무 많아져서 통제 힘듦, 혹은 메모리 부족
arules::apriori(groceries, parameter = # minlen은 요구되는 최소 규칙 아이템
                  list(support = 0.1, confidence = 0.8, minlen = 1))우 # default setting -> nothing learned

# set better support and confidence levels to learn more rules
# 최소 지지도 임계치를 다루는 방법
  # ex)한달 동안 하루 2번 구입되면 60회, 9835회 중 60회는 0.006
# 최소 신뢰도 임계치를 다루는 방법(일단 0.25로 시작)
  # 너무 낮으면 규칙 너무 많아짐, 너무 높으면 뻔한 규칙
groceryrules <- apriori(groceries, parameter = list(support =
                          0.006, confidence = 0.25, minlen = 2))
groceryrules # 규칙 463

## Step 4: Evaluating model performance ----
# summary of grocery association rules
summary(groceryrules)
# 대부분 또는 모든 규칙이 최소 임계치 바로 근처에서 지지도와 신뢰도를 갖는다면
  # 기준을 너무 높게 설정했다는 것을 의미 -> 재조정 필요
  # lift(X -> Y) = confidence(X ->Y)/support(Y)
    # 향상도는 어떤 아이템(집합) X가 구매됐다는 것을 안다면, 다른 아이템(집합)Y가 어떤 확률로 구매될 것인가를
    # Y의 일반적인 구매 확률과 비교해 측정한다.
    # 아이템 순서가 중요한 신뢰도와는 달리, 향상도는 순서가 달라도 동일하다.

# look at the first three rules
inspect(groceryrules[1:3])
# 논리적으로 potted plant -> whole milk는 말이 안되지만, 데이터는 이 규칙이 믿을만 하다고 말하고 있다.
# 일반적인 방법은, 연관 규칙을 받아 다음 세 가지 범주로 규칙을 나누는 것이다.
  # 실행 가능한
  # 사소한(기저귀 -> 분유)
  # 설명하기 어려운(연관성 불명확. 단순히 데이터에 있는 랜덤 패턴일 수도 있다.)
# 충분한 시간이 있다면 숨겨진 보석 규칙을 위해 모든 규칙을 평가할 수 있다.
  # 하지만 분석가는 규칙이 어떤지까지는 잘 모를 수도 있다.
  # 가장 흥미로운 결과가 상단에 표시되도록, 학습된 규칙을 정렬하고 공유하자.


## Step 5: Improving model performance ----

# sorting grocery rules by lift
  # sort()함수로 by 파라미터에 lift를 넣어서, lift가 가장 높은 순서대로 5개를 출력시켰다. 
inspect(sort(groceryrules, by = "lift")[1:5])

# finding subsets of rules containing any berry items
# subset()함수는 거래, 아이템, 규칙의 부분집합을 찾는 방법을 제공한다.
berryrules <- subset(groceryrules, items %in% "berries") # 베리가 들어간 규칙만 부분집합으로 뽑음
inspect(berryrules) # 규칙 출력. lift 높은 두 규칙은 실행 가능해 보인다.
# subset() 사용법
  # items %in% 쓰면 좌측, 우측 모두 포함되기만 하면 뽑는다.
    # 좌측에만 해당되게 하려면 item 대신 lhs, 우측은 rhs
  # 연산자 %in%은 아이템 중 최소 하나가 정의된 목록에서 발견돼야만 한다.
    # 두 개 하고 싶으면 %in% c("berries", "yogurt")로 넣자.
  # %ain%으로 완전 매칭(베리, 요거트 둘 다 있는 규칙만 찾기)
  # %pin%으로 부분 매칭(%pin% fruit로 그냥 과일이랑 열대 과일 같이 찾기)
  # 부분집합은 support, confidence, lift로 제약 가능
  # 매칭 조건은 %, |, !같은 표준 R 논리 연산자와 결합 가능


# writing the rules to a CSV file
write(groceryrules, file = "groceryrules.csv",
      sep = ",", quote = TRUE, row.names = FALSE)

# converting the rule set to a data frame
groceryrules_df <- as(groceryrules, "data.frame")
str(groceryrules_df)
