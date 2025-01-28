# 초기 환경 설정
rm(list=ls())
rm(list=lsf.str())
rm(list=setdiff(ls(), lsf.str()))
dev.off()   
cat("\014")
getwd()
setwd('D:/RRR/financial_mathmatics')
remove.packages('readxl')

Bootstrap.Bond <- function(T) {
  package.list = c("readxl")
  for(i in package.list){
    package.path <- find.package(i,quiet=TRUE)
    if(length(package.path) == 0) {install.packages(i)}
  }
  library(readxl)
  TABLE <- read_excel('HW06_TABLE.xlsx')
  F <- t(TABLE[,1])
  t <- t(TABLE[,2])
  c <- t(TABLE[,3])
  m <- t(TABLE[,4])
  PT <- t(TABLE[,5])
  df <- t(TABLE[,6]) # 값 모두 NA
  zr <- t(TABLE[,7]) # 값 모두 NA
  coupon <- t(F*(c/m)) # 타이핑 덜 하기 위해 미리 정의하였음
  for (i in 1:length(t)) {
    if (c[i]==0) {
      df[i] <- PT[i]/F[i]
    } else {
      df[i] <- (PT[i] - coupon[i]*sum(df[1:(i-1)]))/(F[i] + coupon[i])}
    zr[i] <- -log(df[i])/t[i]
  }
  f <- approxfun(t,zr)
  if (T>2) {print(max(zr))} else if (T>=0.5) {print(f(T))} else {print(min(zr))}
}

Result <- data.frame(t = NA, zr = NA)
j <- 1
for (i in seq(0,2.5,(1/365))) {
  Result[j,1] <- i
  zr <- Bootstrap.Bond(i)
  Result[j,2] <- zr
  j <- j+1
}

View(Result); plot(Result)
write.csv(Result, file='HW07d_13123313.csv',
          row.names=TRUE)
