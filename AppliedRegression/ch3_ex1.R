# cor() 함수는 상관분석을 수행한다.
# method = c("pearson", "kendall", "spearman") 기본값은 pearson이다.

# longley 자료에 대해 상관분석 수행
data(longley); force(longley)
str(longley); pairs(longley)

# 스피어만 상관계수는 원자료 대신 순위 자료를 사용한다
  # 피어슨 상관계수보다 이상치 자료에 대해 덜 민감하게 반응
# 두 상관계수의 차이가 큼 -> 두 변수 간의 비선형성 의심
cor(longley, method = "spearman")

# 켄달의 타우는 일종의 비모수적 상관계수
cor(longley, method = "kendall")

# 상관계수에 대한 검정은 cor.test() 함수 사용.
## Default S3 method:
cor.test(x, y,
         alternative = c("two.sided", "less", "greater"),
         method = c("pearson", "kendall", "spearman"),
         exact = NULL, conf.level = 0.95, continuity = FALSE, ...)

## S3 method for class 'formula'
cor.test(formula, data, subset, na.action, ...)
# 이 가운데 피어슨 상관계수에 대한 검정은 t-검정을 사용한다.(p.33)