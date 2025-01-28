# 초기 환경 설정
rm(list=ls())
rm(list=lsf.str())
rm(list=setdiff(ls(), lsf.str()))
dev.off()   
cat("\014")

Bond.P <- function(F,T,c,m,y) {
  r = 1+(y/m)
  n = m*T
  p.coupon = F*(c/y)*(1-(1/r^n))
  p.principal = F*(1/r^n)
  ans <- p.coupon + p.principal
  return(ans)
}

# 듀레이션 : sum(PV*t)/sum(PV), 수정듀레이션 : 듀레이션*(1/1+y)
Bond.D <- function(F,T,c,m,y){
  r = 1+(y/m)
  n = m*T
  p.coupon = F*(c/y)*(1-(1/r^n))
  p.principal = F*(1/r^n)
  price <- p.coupon + p.principal
  boonja = sum((1:n)*((F*c/m)/((1+y/m)^(1:n)))) + p.principal*n
  dap = (boonja/price)*(1/m)
  return(dap)
}
# 수정 듀레이션 구하기
Bond.MD <- function(F,T,c,m,y) {
  r = 1+(y/m)
  n = m*T
  p.coupon = F*(c/y)*(1-(1/r^n))
  p.principal = F*(1/r^n)
  price <- p.coupon + p.principal
  boonja = sum((1:n)*((F*c/m)/((1+y/m)^(1:n)))) + p.principal*n
  dap = (boonja/price)*(1/(1+y/m))*(1/m)
  return(dap)
}
# 볼록성 구하기
Bond.C <- function(F,T,c,m,y) {
  r = 1+(y/m)
  n = m*T
  p.coupon = F*(c/y)*(1-(1/r^n))
  p.principal = F*(1/r^n)
  price <- p.coupon + p.principal
  boonja = sum((1:n)*(2:(n+1))*((F*c/m)/((1+y/m)^(1:n)))) + p.principal*n*(n+1)
  dap = (boonja/price)*(1/(1+y/m)^2)*(1/m^2)
  return(dap)
}

# 결과값 행렬 구하기
P0 <- Bond.P(10000,5,0.05,4,0.1)
MD <- Bond.MD(10000,5,0.05,4,0.1)
C <- Bond.C(10000,5,0.05,4,0.1)
names <- c('-4%', '-2%', '2%', '4%')
actual <- c((Bond.P(10000,5,0.05,4,0.06)-P0),(Bond.P(10000,5,0.05,4,0.08)-P0),(Bond.P(10000,5,0.05,4,0.12)-P0),(Bond.P(10000,5,0.05,4,0.14)-P0))
with.MD <- c((-MD*P0*(-0.04)),(-MD*P0*(-0.02)),(-MD*P0*0.02),(-MD*P0*0.04))
with.MD.C <- c((-MD*P0*(-0.04)+0.5*C*P0*(-0.04)^2),(-MD*P0*(-0.02)+0.5*C*P0*(-0.02)^2),(-MD*P0*(0.02)+0.5*C*P0*(0.02^2)),(-MD*P0*(0.04)+0.5*C*P0*(0.04^2)))

Result <- rbind(names, actual, with.MD, with.MD.C) # 실수...4*4행렬 만들어버림
View(Result)

Result <- rbind(actual, with.MD, with.MD.C) # 이렇게 만들었어야
colnames(Result) <- names
View(Result)
