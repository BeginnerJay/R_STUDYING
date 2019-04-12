##### Chapter 4: Classification using Naive Bayes --------------------
### 베이즈 정리를 이용한 조건부 확률 계산
## P(A|B) = ( ( P(B|A) * P(A) ) / P(B) ) 베이즈 정리
# P(A|B) = 사후 확률
# P(B|A) = 우도(Likelihood)
# P(A) = 사전 확률
# P(B) = 주변 우도

## 나이브 베이즈 알고리즘
# 나이브 베이즈는 데이터셋의 모든 특징이 동등하게 중요하고, 독립적이라고 가정한다.
# 이런 가정은 현실과는 거리가 있지만, 이런 가정이 맞지 않는 대부분의 경우에도 나이브 베이즈는 잘 작동한다.
# 조건부확률 공식은 변수가 늘어나면 엄청나게 복잡해지지만
# 독립사건에 대한 규칙 P(A&B) = P(A) * P(B)를 이용하면 단순하게 표현할 수 있따.

## 라플라스 추정량
# 한 개 이상의 클래스 레벨에서 사건이 절대 발생하지 않는다면
# 다른 사건이 어떻든 간에 사후 확률이 0이 되는 문제가 생긴다.
# 라플라스 추정량으로 이것을 해결한다(빈도표의 각 합계에 작은 수를더해 준다)
# 보통 라플라스 추정량은 1로 설정해서, 데이터에 각 클래스 특징 조합이 최소 한 번은 나타나게 보장한다.

## 나이브 베이즈에서 수치 특성 이용
# 나이브 베이즈는 데이터를 학습하기 위해 빈도표를 사용하기 때문에
# 행렬을 구성하는 각 클래스와 특징 값의 조합을 생성하려면 각 특징이 범주형이여야 한다.
# 수치 특징을 이산화하는 것이 해결 방법이 될 수 있다.(bins라는 범주에 숫자를 넣기)
# 이산화하면 범주가 작아지기 때문에 정보가 손실된다. 이 균형을 맞추는 것이 중요하다.
# 빈의 개수가 너무 적으면 중요한 추세가 모호해지고
# 빈의 개수가 너무 많으면 알고리즘의 잡음이 커진다.

getwd()
setwd("D:/R_LAB/MLwR/Chapter 04")
sms_raw <- read.csv("sms_spam.csv", stringsAsFactors = F)
str(sms_raw)
# type 항목을 범주형 변수로 변환해준다
sms_raw$type <- factor(sms_raw$type)
table(sms_raw$type)
if(!require(tm)) {
  install.packages("tm")
  require(tm)
}
# 텍스트 데이터 처리츼 엇 단계는 텍스트 문서의 모음인 코퍼스(corpus)를 생성하는 것이다.
# 코퍼스를 생성하기 위해 휘발성(volatile) 코퍼스를 참조하는 tm::VCourpus()한수 사용
# sms_raw$text 벡터에서 VectorSource 리더 함수를 이용햐 소스 객체를 생성한 후, VCorpus에 제공한다.
sms_corpus <- VCorpus(VectorSource(sms_raw$text))
print(sms_corpus)
# 기본적으로 tm 코퍼스는 복합 리스트이기 때문에, 코퍼스에서 리스트 연산을 사용해서 문서를 선택할 수 있다.
# 특정 메시지의 요약을 얻으려면 리스트 연산자와 함께 inspect() 함수 사용한다.
# ex) 다음 명령으로 코퍼스의 첫 번째, 두 번째 SMS 메시지 요약 볼 수 있음.
inspect(sms_corpus[1:2])
# 실제 메시지 텍스트를 보려면 as.character() 함수를 원하는 메시지에 적용한다.
# 한 개의 메시지를 보려면 리스트의 한 항목에 as.character() 함수를 사용한다.
# 단, 이중 괄호 표시를 해야 한다.
as.character(sms_corpus[[1]])
# 여러 문서를 보려면 sms_corpus 객체의 여러 항목에 as.character()를 적용해야 한다.
# 그러기 위해 R 데이터 구조의 항목별로 함수를 적용하는 lapply() 활용한다.
# lapply(), apply, sapply()는 가독성이 좋은 코드 생산 -> for, while 루프 사용하듯 사용

# 분석을 하려면 메시지를 개별 단어로 나눠야 한다.
# 단어를 표준화하려면, 먼저 구두점 정리하고, 대소문자 정리해 준다.
# tm_map() 함수는 tm 코퍼스에 (변환)매핑 적용하게 해 준다.
# 이 함수를 이용해, 일련의 변환을 거쳐 코퍼스를 정리하고, corpus_clean이라고 하는 새로운 객체에 결과를 저장한다.
# 소문자만 사용하도록 메시지 표준화
# tm 래퍼 함수 content_transformer()를 통해 tolower()를 코퍼스에 적용한다.
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))

# show the difference between sms_corpus and corpus_clean
as.character(sms_corpus[[1]])
as.character(sms_corpus_clean[[1]])

sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers) # remove numbers
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords()) # remove stop words
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation) # remove punctuation

# tip: create a custom function to replace (rather than remove) punctuation
removePunctuation("hello...world")
replacePunctuation <- function(x) { gsub("[[:punct:]]+", " ", x) }
replacePunctuation("hello...world")

# illustration of word stemming
library(SnowballC)
wordStem(c("learn", "learned", "learning", "learns"))

sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)

sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace) # eliminate unneeded whitespace

# examine the final clean corpus
lapply(sms_corpus[1:3], as.character)
lapply(sms_corpus_clean[1:3], as.character)

# create a document-term sparse matrix
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)

# alternative solution: create a document-term sparse matrix directly from the SMS corpus
sms_dtm2 <- DocumentTermMatrix(sms_corpus, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  stopwords = TRUE,
  removePunctuation = TRUE,
  stemming = TRUE
))

# alternative solution: using custom stop words function ensures identical result
sms_dtm3 <- DocumentTermMatrix(sms_corpus, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  stopwords = function(x) { removeWords(x, stopwords()) },
  removePunctuation = TRUE,
  stemming = TRUE
))

# compare the result
sms_dtm
sms_dtm2
sms_dtm3

# creating training and test datasets
sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test  <- sms_dtm[4170:5559, ]

# also save the labels
sms_train_labels <- sms_raw[1:4169, ]$type
sms_test_labels  <- sms_raw[4170:5559, ]$type

# check that the proportion of spam is similar
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

# word cloud visualization
library(wordcloud)
wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE)

# subset the training data into spam and ham groups
spam <- subset(sms_raw, type == "spam")
ham  <- subset(sms_raw, type == "ham")

wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))

sms_dtm_freq_train <- removeSparseTerms(sms_dtm_train, 0.999)
sms_dtm_freq_train

# indicator features for frequent words
findFreqTerms(sms_dtm_train, 5)

# save frequently-appearing terms to a character vector
sms_freq_words <- findFreqTerms(sms_dtm_train, 5)
str(sms_freq_words)

# create DTMs with only the frequent terms
sms_dtm_freq_train <- sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_words]

# convert counts to a factor
convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

# apply() convert_counts() to columns of train/test data
sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
sms_test  <- apply(sms_dtm_freq_test, MARGIN = 2, convert_counts)

## Step 3: Training a model on the data ----
library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_train_labels)

## Step 4: Evaluating model performance ----
sms_test_pred <- predict(sms_classifier, sms_test)

library(gmodels)
CrossTable(sms_test_pred, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

## Step 5: Improving model performance ----
sms_classifier2 <- naiveBayes(sms_train, sms_train_labels, laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
