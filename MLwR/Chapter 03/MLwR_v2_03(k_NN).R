##### Chapter 3: Classification using Nearest Neighbors --------------------

## Example: Classifying Cancer Samples ----
## Step 2: Exploring and preparing the data ---- 

# import the CSV file
getwd()
setwd("D:/R_LAB/MLwR/Chapter 03")
wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)

# examine the structure of the wbcd data frame
str(wbcd)

# drop the id feature
wbcd <- wbcd[-1]

# table of diagnosis
table(wbcd$diagnosis)

# recode diagnosis as a factor
# 많은 머신 러닝 분류기는 목표로 하는 특징이 범주형이어야 한다.
# 그래서 diagnosis 변수를 범주형으로 변환한다.
# 그리고 label 파라미터를 통해 B, M을 유용한 정보를 주도록 변환한다
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))

# table or proportions with more informative labels
prop.table(table(wbcd$diagnosis))
# round()는 반올림해주는 함수
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

# summarize three numeric features
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

# knn은 입력 특징이 측정 척도에 의존적 -> 정규화해준ㄷ
# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# test normalization function - result should be identical
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))

# normalize the wbcd data
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
# lapply() 함수는 리스트를 취해서 각 리스트 항목에 지정된 함수를 적용한다.
# 데이터 프레임은 동일한 길이의 벡터들의 리스트이므로,
# data.frame의 각 특징에 normalize()를 적용하려면 lapply()를 이용한다.
# 그리고 lapply()로 인해 list로 반환된 것들을 다시 as.data.frame()으로 data.frame으로 바꿔준다.


# confirm that normalization worked
summary(wbcd_n$area_mean)

# create training and test data
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]
# wbcd는 이미 무작위로 정렬되어 있었기 때문에, 테스트 데이터 셋을 생성할 때에 연속적으로 해도 무방.
# 시간순으로 정렬되어 있다면 랜덤 샘플링 필요(5장)


# create labels for training and test data

wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

## Step 3: Training a model on the data ----
# knn같은 게으른 학습자는 단순히 입력 데이터를 구조화된 형식으로 저장할 뿐, 모델을 실제로 구축하지는 않는다.


# load the "class" library
library(class)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k = 21)
# cl = class : 훈련 데이터의 각 행에 대한 클래스를 갖는 팩터 벡터
# k = 최근접 이웃의 개수를 가리키는 정수(낮으면 overfit)


## Step 4: Evaluating model performance ----

# load the "gmodels" library
install.packages("gmodels")
library(gmodels)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq = FALSE)
# CrossTable() 함수로 두 벡터 간의 일치를 나타내는 교차표를 생성
# prop.chisq = FALSE 로 결과에 필요 없는 카이제곱 값을 제거한다.

## Step 5: Improving model performance ----

# use the scale() function to z-score standardize a data frame 정규화 대신
wbcd_z <- as.data.frame(scale(wbcd[-1]))
# scale() <- z-표준화 함수. data.frame에 직접 적용이 가능하기 때문에 lapply 적용할 필요가 없다.

# confirm that the transformation was applied correctly
summary(wbcd_z$area_mean)

# create training and test datasets
wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]

# re-classify test cases
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k = 21)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq = FALSE)

# try several different values of k
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=1)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=5)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=11)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=15)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=27)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)
# 1-nn이 거짓 부정을 피하기 용이하다.
# 하지만 k 갯수를 너무 낮추는 것은 좋지 않다.

## 요약
# k-NN은 사실 어떤 학습도 하지 않는다. 훈련 데이터를 특정한 방식으로 저장할 뿐이다.
# 아주 단순한 알고리즘이지만, 극단적으로 복잡한 작업도 대체가 가능하다.
# 4장에서는, 관측이 특정 범주에 속할 가능성을 평가하는, 확률 기반의 분류 방법을 살펴본다.
# 9장에서, 완전히 다른 학습 작업에 거리 척도를 사용하는, k-NN과 유사한 방법에 대해 다룬다.