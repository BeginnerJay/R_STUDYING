install.packages("psych"); library(psych)
# 상관행렬의 시각화를 위해 psych::pairs.panel() 함수와 psych::cor.plot() 함수를 이용한다.
  # pairs.panels() : 산점도와 함께 모든 변수들간 상곤계수. 작을때 사용
  # cor.plot() : 이미지 제공. 수가 많을 때 사용
data(iris)
pairs.panels(iris[1:4], scale = T) # scale=T 하면 상관계수 크기에 따라 글자크기 달라짐
pairs.panels(iris[1:4], bg = c("red", "yellow", "blue")[iris$Species], # 그룹별로 색상지정
             pch = 21, main = "Fisher Iris data by Species")

cor.plot(cor(mtcars))

install.packages("corrplot"); library(corrplot)
# corrplot::corrplot() : 상관행렬, 신뢰구간
# corrplot::mixed() : 혼합방법으로 시각화
M <- cor(mtcars)
corrplot::corrplot(M, method = "ellipse", type = "lower")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method = "color", col = col(200),
         type = "upper", addCoef.col = "black",
         tl.srt = 45, tl.col = "black", diag = FALSE)
par(mfrow = c(1,1))
corrplot.mixed(M)

# 상관행렬의 재배열(숨겨진 구조와 패턴의 마이닝을 우해 중요)
  # AOE : 고유벡터의 각 순서
  # FPC : 제1주성분 순서
  # hclust : 위계적 군집 순서
  # alphabet : 알파벳 순서
corrplot(M, order = "hclust", addrect = 3)

# 컬러 스펙트럼 변경
  # 색상 palette 생성
col.1 <- colorRampPalette(c("red", "white", "blue"))
wb <- c("white", "black")
par(mfrow = c(1,2))
corrplot(M, order = "hclust", addrect = 2, col = col.1(20)) # 색상 세개로부터 20가지 색상스펙트럼 사용
corrplot(M, order = "hclust", addrect = 2, col = wb, bg = "gold2")
# 컬러 범례, 텍스트 범례
corrplot(M, order = "AOE", c1.pos = "b", t1.pos = "d", tl.srt = 60)
