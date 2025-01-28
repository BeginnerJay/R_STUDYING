rm(list=ls())
2^(1:3) # 등비수열
2^2:5; 2^(2:5) # 차이점에 유의

# 채권 가격의 계산

Bond.P <- function(F,T,c,m,y) {
  r = 1+(y/m)
  n = m*T
  p.coupon = F*(c/y)*(1-(1/r^n))
  p.principal = F*(1/r^n)
  ans <- p.coupon + p.principal
  print(p.coupon)
  print(p.principal)
  print(ans)
}
Bond.1 <- Bond.P(10000,5,0.05,1,0.1)
Bond.2 <- Bond.P(10000,7,0.05,1,0.1)
Bond.3 <- Bond.P(10000,5,0.05,2,0.1) # c는 연 지급 이표!!
Bond.P(10000,5,0.05*2,2,0.1) # 멋대로 수정하면 틀린다

F <- 10000
R <- (1+(0.1/2))
N <- (2*5)
P.COUPON <- F*(0.05/0.1)*(1-1/R^N)
P.PRINCIPAL <- F/(R^N)
P.BOND <- P.COUPON + P.PRINCIPAL

