Comment1 = "
	이런 식으로
	안쓰는 문자열을
	주석처럼 쓸 수 있습니다.
	다른 곳에 쓰지만 않으면 되니까
	원하는 만큼 코멘트 쓰고 드래그 해서 블록 만든 후에
	ctrl + shift + c 누르면 모두 # 붙여줌(rstudio)
"

# caret::preProcess() 함수는 개별 예측 변수에 대한 중심화(centering), 척도화(scaling) 및
# 박스-콕스 변환(Box-Cox transformation)을 제공한다.
# (양의 값을 가진 자료에 적용되는 분산 안정화 및 정규화를 위한 변환)

# 두 개의 예측 변수를 가지는 데이터 프레임을 생성하고, 이를 이용하여 전처리 과정을 수행한다
set.seed(13123313)
predictors <- data.frame(x1 = rnorm(1000, mean = 5, sd = 2), # 정규분포
                         x2 = rexp(1000, rate = 10))# 지수분포
par(mfrow = c(1,3))
plot(predictors); hist(predictors[,1]); hist(predictors[,2])

install.packages("caret"); library(caret)
trans <- preProcess(predictors, c("BoxCox", "center", "scale"))
predictorsTrans <- data.frame(trans = predict(trans, predictors))
par(mfrow = c(1,3))
plot(predictorsTrans); hist(predictorsTrans[,1]); hist(predictorsTrans[,2])

# R에서 박스콕스변환 하는 함수는 MASS::boxcox(), car::bcPower(), car::powerTransform() 등이 있다.
# 나종화 2권에서 알아보자.
# 자료의 변환을 포함한 R에서 전처리 함수에는 base::scale(), pcaPP::ScaleAdv(), 
# caret::PreProcess(), sparseLDA::normalize() 등이 있다.
# 이 가운데 처음 3개 함수는 중심화와 척도화만 제공한다.
# PreProcess는 매우 다양한 예측변수에 대한 전처리를 제공

# 분산안정화 변환
  # 비율 또는 이항 자료 : 역사인 또는 로짓 변환
  # 개수 또는 포아송 자료 : 제곱근(또는 Anscombe) 변환
  # 표본상관계수 : 피셔의 변환
  # 회귀분석에서의 박스-콕스 변환