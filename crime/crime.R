##### 데이터 불러오기 #####

library('openxlsx')
myData <- read.xlsx(xlsxFile = "D:/GitHub/R_STUDYING/crime/adjusted.xlsx")
str(myData) # 문자형
myData$자치구 <- as.factor(myData$자치구); str(myData) # 자치구 변수를 문자형에서 범주형으로 변경


##### 상관분석 #####

crimeDensityCorr <- as.data.frame(cor(myData[2:16]))  # 각 변수들과 총 범죄밀도 사이의 상관관계 행렬 
plot(crimeDensityCorr)
crimeDensityCorr[15, ]

# 지구대및파출소, 순찰차, 경관의 수는 오히려 범죄밀도와 양의 상관관계가 있다.
# 이는 이러한 변수가 범죄를 억지하는 요인이 아닌, 범죄가 늘어남에 따라 늘어나는 결과적 변수일 수 있음을 알린다.
# 검거율은 억지 요인으로써 음의 상관관계를 보이고 있다.
# 외국인 비율은 양의 상관관계, 외국인 혼인율은 음의 상관관계를 보이고 있다.

felonyDensityCorr <- as.data.frame(cor(myData[c(2:15, 17)])) # 총 범죄밀도 빼고 흉악범죄밀도 추가
plot(felonyDensityCorr)
felonyDensityCorr[15,]

scDensityCorr <- as.data.frame(cor(myData[c(2:15, 18)])) # 총 범죄밀도 빼고 성범죄 밀도 추가
plot(scDensityCorr)
scDensityCorr[15,]

# 젊은 여자 1인가구와의 상관관계가 0.15에서 0.54로 크게 증가했다.
# 성범죄밀도에서는 지구대-파출대 밀도와 음의 상관관계를 보이고 있다.


##### 회귀분석 #####

### 우선 특별한 처리 없이 시행 ###

#crimeDensityReg <- as.data.frame(myData[2:16]) # 자치구가 범주형이라 일단 뺌.
#Reg1 <- lm(formula = 범밀도 ~. , data = crimeDensityReg) # ~.은 모든 변수 다쓰겠다는말. 
#summary(Reg1) 
# p-value가 다 엉망으로 나와서 전처리 추가시행(min-max normalization)
# 데이터가 동일한 정도의 scale(중요도)로 반영되도록 하는 것이 정규화의 목표.
#normalize <- function(x) {
#  return((x - min(x)) / (max(x) - min(x)))
#}
# min-max 정규화. crimeDensityReg <- normalize(crimeDensityReg)

#crimeDensityReg <- as.data.frame(scale(crimeDensityReg)) # Z-점수 정규화. 표준화라고도 함.
#Reg1 <- lm(formula = 범밀도 ~. , data = crimeDensityReg)
#summary(Reg1)

# 필요 없는 변수들을 줄여보자. 
library(car)
crimeDensityReg <- as.data.frame(myData[2:16])
crimeDensityReg <- as.data.frame(scale(crimeDensityReg))
Reg1 <- lm(formula = 범밀도 ~. , data = crimeDensityReg)
Reg1_step <- step(Reg1, direction = "both")
summary(Reg1_step)
vif(Reg1_step) # 1가구율이 다중공선성이 약간 높지만(5이상), 10 이하여서 일단은 괜찮다고 본다.

felonyDensityReg <- as.data.frame(scale(myData[c(2:15, 17)]))
Reg2 <- lm(formula = 흉밀도 ~. , data = felonyDensityReg)
Reg2_step <- step(Reg2)
summary(Reg2_step); vif(Reg2_step)

scDensityReg <- as.data.frame(scale(myData[c(2:15, 18)]))
Reg3_step <- step(lm(formula = 성밀도 ~. , data = scDensityReg))
summary(Reg3_step); vif(Reg3_step)

# 회귀분석에서도 순찰차 밀도는 범죄와 오히려 양의 관계가 있음을 보인다.
# 검거율은 억지 요인으로서 인과관계가 있음을 보였다.
# 젊은여자1인가구비율은 성범죄에서는 양의 인과관계를 보였지만, 그 외 범죄에서는 음의 관계를 보였다.
# 추측하기에는 다른 범죄보다 성범죄에 우선적으로 노출되어서 그런 것인가? 하는 생각.