##### Chapter 9: Clustering with k-means -------------------

# 군집화는 데이터를 cluster로 자동 분리하는 머신 러닝 작업이다.
# 찾고 있는 것이 무엇인지 모르기 때문에, 군집화는 예측보다는 지식의 발견에 사용된다.

# 모델은 데이터 내에 존재하는 패턴을 설명한다.
# 이와 대조적으로 군집화는 새로운 데이터를 생성한다.

# 주목할 점은 자율 분류기(=군집기)에서 얻은 클래스 레이블은 본질적인 의미가 없다는 것이다.
  # 군집화는 어떤 예시 그룹이 긴밀하게 연관되어 있는지는 말해 주지만,
  # 실행 가능하고 의미 있는 레이블을 적용하는 것은 사람의 책임이다.

# k- 평균 알고리즘은 n개의 예시를 k개의 클러스터 중 하나에 할당하는데, 이 때 k는 사전에 결정됨
  # 이 알고리즘의 목표는 클러스터 내의 차이를 최소화하고, 클러스터 간의 차이를 최대화하는 것이다.

# k-평균 알고리즘은 클러스터 중심의 출발 위치에 매우 민감하다.
  # k-평균의 초기 중심을 선택하는 방법을 다르게 할 수 있다. 거리는 보통 유클리드 거리를 사용한다.

# k를 크게 설정하면 클러스터의 동질성이 향상 but overfitting 위험 커진다.
  # 이상적으로는 실제 그룹에 대한 선험적(a priori)지식이 있을 것이며, 이 정보를 클러스터 개수를 선택하는데 적용가능
  # 사전 지식이 없다면 k를 n/2의 제곱근과 동일하도록 설정해보자.
  # 엘보법
    # 다양한 k값에 대해 클러스터 내의 동질성과 이질성이 어떠헥 변하는지를 측정한다.
## Example: Finding Teen Market Segments ----
## Step 2: Exploring and preparing the data ----
setwd("D:/R_LAB/MlwR/Chapter 09")
teens <- read.csv("snsdata.csv")
str(teens)

# look at missing data for female variable
table(teens$gender)
table(teens$gender, useNA = "ifany")

# look at missing data for age variable
summary(teens$age)

# eliminate age outliers
teens$age <- ifelse(teens$age >= 13 & teens$age < 20,
                     teens$age, NA)

summary(teens$age)

# reassign missing gender values to "unknown"
teens$female <- ifelse(teens$gender == "F" &
                         !is.na(teens$gender), 1, 0)
teens$no_gender <- ifelse(is.na(teens$gender), 1, 0)

# check our recoding work
table(teens$gender, useNA = "ifany")
table(teens$female, useNA = "ifany")
table(teens$no_gender, useNA = "ifany")

# finding the mean age by cohort
mean(teens$age) # doesn't work
mean(teens$age, na.rm = TRUE) # works

# age by cohort
# aggregate() 함수는 데이터의 하위 그룹에 대한 통계를 계산한다.
aggregate(data = teens, age ~ gradyear, mean, na.rm = TRUE)

# create a vector with the average age for each gradyear, repeated by person
# ave() 함수는 결과가 원 벡터의 길이와 같아지게 그룹의 평균이 반복되는 벡터를 반환한다.
ave_age <- ave(teens$age, teens$gradyear,
                 FUN = function(x) mean(x, na.rm = TRUE))


teens$age <- ifelse(is.na(teens$age), ave_age, teens$age) # 결측치에 ave_age 넣

# check the summary results to ensure missing values are eliminated
summary(teens$age)

## Step 3: Training a model on the data ----
interests <- teens[5:40]
interests_z <- as.data.frame(lapply(interests, scale))

set.seed(13123313)
teen_clusters <- kmeans(interests_z, 5)

## Step 4: Evaluating model performance ----
# look at the size of the clusters
teen_clusters$size

# look at the cluster centers :각 클러스터의 동질성을 살펴보자. 중심점의 좦를 알아보자.
teen_clusters$centers
# 양수는 모든 십대에 대한 전체 평균 이상, 음수는 전체 평균 이하.
# 클러스터가 각 관심 범주에 대해 평균 이상인지 이앟인지 검토 > 클러스터는 서로 구별하는 패턴 인식.


## Step 5: Improving model performance ----
# apply the cluster IDs to the original data frame

# 이 클러스터를 전체 데이터셋에 역을 ㅗ적용하자.
# kmeans() 함수에 의해 생성된 teen_cluster객체는 cluster라는 이름의 component를 포함한다.
# cluster component에는 전체 30000명 개인에 대한 클러스터 할당이 포함되어 있다.
# 다음 명령으로 cluster component를 teens 데이터 프레임의 열로 추가할 수 있다.
teens$cluster <- teen_clusters$cluster

# look at the first five records
teens[1:5, c("cluster", "gender", "age", "friends")]

# mean age by cluster # aggregate()로 인구통계학적 특성 살펴보
aggregate(data = teens, age ~ cluster, mean)

# proportion of females by cluster
aggregate(data = teens, female ~ cluster, mean) # 여성 비율은 상당한 차이가 있다(성별 데이터 사용 않았음에도)

# mean number of friends by cluster
aggregate(data = teens, friends ~ cluster, mean) # 공주, 범죄자, 그리고 무력한 사람

# k-평균 알고리즘 뿐만 아니라, 작업에 고유한 편향과 휴리스틱을 제공하는 다른 여러 군집 알고리즘 변형이 있다.