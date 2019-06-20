# cats 자료에 대해 상관분석 수행
install.packages("MASS"); library(MASS)
data(cats); str(cats); summary(cats)
par(mfrow = c(1,1))
with(cats, plot(Bwt, Hwt)); title(main = "Heart Weight(g) vs. Body Weight(kg)\n of Domestic Cats")
# (Bwt ~ Hwt)와 동일
pearsonCor <- with(cats, cor(Bwt, Hwt))
coef_det <- pearsonCor^2; coef_det # 단순회귀에서는 결정계수는 상관계수의 제곱
with(cats, cor.test(~ Bwt + Hwt)) # (Bwt, Hwt)와 동일
with(cats, cor.test(~ Bwt + Hwt, subset = (Sex == "F"))) # cor.test()에는 공식을 사용할 수도 있다.
# F일때는 0.5 <-> 전체 0.8와 큰 차이
par(mfrow = c(1,2))
with(cats, plot(Bwt, Hwt,
                type = "n",
                xlab = "Body Weight in kg",
                ylab = "Heart weight in g",
                main = "Heart Weight(g) vs. Body Weight(kg)\n of Domestic Cats"))
with(cats, points(Bwt[Sex=="F"], Hwt[Sex=="F"], pch = 16, col = "red"))
with(cats, points(Bwt[Sex=="M"], Hwt[Sex=="M"], pch = 17, col = "blue"))