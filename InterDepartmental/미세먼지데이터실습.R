## 1단계 : 데이터 구하기
wind <- read.csv(file = "D:/R_LAB//전체데이터-강화_201901to03_풍향자료.csv")
str(wind)

normalize <- function(x) {
  return( (x - min(x)) / (max(x) - min(x)) )
}
## 2단계 : 탐색, 준비
#lapply를 통해 모든 열에 normalize() 함수를 적용한다.(sapply()와는 다르게 리스트로 반환)
concrete_norm <- as.data.frame(lapply(concrete, normalize))
str(concrete_norm); summary(concrete_norm)
concrete_train <- concrete_norm[1:773, ] # 1에서 773번째 행 구성요소 전체 선택, 훈련용
concrete_test <- concrete_norm[774:1030, ]

install.packages("neuralnet")
library(neuralnet)
## 3단계 : 훈련
# 은닉 노드가 하나뿐인 가장 단순한 다층 순방향 네트워크부터 시작
set.seed(13123313)
concrete_model <- neuralnet(strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age,
                            data = concrete_train)
# 네트워크 토폴로지의 시각화
plot(concrete_model)
# 특징별로 입력 노드가 하나씩
# 그 뒤를 이어 하나의 은닉 노드와 콘크리트 강도를 예측하는 하나의 출력 노드.

## 4단계 : 모델 성능 평가
# 테스트 데이터셋에 대해 예측을 생성하려면 compute()
# compute()는 두 개의 구성 요소로 된 리스트 반환.
# $neurons는 네트워크의 계층별로 뉴런 저장
# $net.result는 예측 값 저장

model_results <- neuralnet::compute(concrete_model, concrete_test[1:8])
predicted_strength <- model_results$net.result
# 분류 문제에서는 모델의 정확도를 검토할 때에 혼동 행렬을 사용할 수 있다.
# 회귀(수치예측) 문제에서는 모델의 정확도를 검토할 때에 혼동 행렬을 사용할 수 없다.

# 두 수치 벡터 간의 상관관계 구하기 -> cor() 사용
cor(predicted_strength, concrete_test$strength)

## 5단계 : 모델 성능 개션
set.seed(13123313)
concrete_model_2 <- neuralnet(strength ~ cement 
                              + slag 
                              + ash 
                              + water 
                              + superplastic 
                              + coarseagg 
                              + fineagg 
                              + age,
                              data = concrete_train,
                              hidden = 5)
plot(concrete_model_2) # Error(SSE) 감소하고 훈련횟수 증가
model_results_2 <- neuralnet::compute(concrete_model_2, concrete_test[1:8])
predicted_strength_2 <- model_results_2$net.result
cor(predicted_strength_2, concrete_test$strength)
