##### 데이터 불러오기 #####

library('openxlsx')
myData <- read.xlsx(xlsxFile = "D:/GitHub/R_STUDYING/crime/adjusted.xlsx")
str(myData) # 문자형
myData$자치구 <- as.factor(myData$자치구); str(myData) # 자치구 변수를 문자형에서 범주형으로 변경


##### 상관분석 #####

crimeDensityCorr <- cor(myData[2:16]) # 각 변수들과 총 범죄밀도 사이의 상관관계 행렬 
crimeDensityCorr <- as.data.frame(crimeDensityCorr)
crimeDensityCorr <- crimeDensityCorr[15, ]

# 지구대및파출소, 순찰차, 경관의 수는 오히려 범죄밀도와 양의 상관관계가 있다.
# 이는 이러한 변수가 범죄를 억지하는 요인이 아닌, 범죄가 늘어남에 따라 늘어나는 결과적 변수일 수 있음을 알린다.
# 검거율은 억지 요인으로써 음의 상관관계를 보이고 있다.
# 외국인 비율은 양의 상관관계, 외국인 혼인율은 음의 상관관계를 보이고 있다.

scDensityCorr <- cor(myData[c(2:15, 18)]) # 총 범죄밀도는 빼고, 성범죄 밀도 추가
scDensityCorr <- as.data.frame(scDensityCorr)
scDensityCorr <- scDensityCorr[15,]

# 젊은 여자 1인가구와의 상관관계가 0.15에서 0.54로 크게 증가했다.
# 성범죄밀도에서는 지구대-파출대 밀도와 음의 상관관계를 보이고 있다.

FelonyDensityCorr <- cor(myData[c(2:15, 17)]) # 총 범죄밀도 빼고 흉악범죄밀도 추가
FelonyDensityCorr <- as.data.frame(FelonyDensityCorr)
FelonyDensityCorr <- FelonyDensityCorr[15,]

 
##### 회귀분석 #####

### 우선 특별한 처리 없이 시행 ###

