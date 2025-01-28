rm(list=ls()) #clearing of datasets in Rstudio Environment
rm(list=lsf.str())
rm(list=setdiff(ls(), lsf.str()))
dev.off()     #clearing of plots in Rstudio
cat("\014")   #clearing of console in Rstudio

#### 7.5 옵션(기초)

### 7.5.1 옵션 개요

f.call <- function(x) sapply(x, function(x) max(c(x-K,0)))
f.put <- function(x) sapply(x, function(x) max(c(K-x,0)))
K <- 200
curve(f.call, 150, 250, col = 'blue', lty = 1, lwd = 1, ylab = expression(f(x)))
curve(f.put, 150, 250, col = 'red', add = TRUE, lty = 2, lwd = 2)
legend('top', c('call','put'), lty = c(1,2), lwd = c(1,2), col = c('blue', 'red'), bty = 'n')

### 7.5.2 블랙숄즈 공식

## (1) 콜옵션 가격
# s : 기초자산의 가격, k = 행사가격, r : 무위험이자율, q : 배당률, v : 변동성, t : 만기
bso <- function(s,k,r,q,v,t) {
  d1 <- (log(s/k) + (r-q+0.5*(v^2))*t)/(v*(t^0.5)); d2 <- d1 - v*sqrt(t)
  nd1 <- pnorm(d1); nd2 <- pnorm(d2)
  ans <- s*exp(-q*t)*nd1 - k*exp(-r*t)*nd2
  return(ans)
}
bso(100,100,0.04,0,0.2,0.5)
# 변동성에 따른 옵션 가치의 변화
s <- 100; r <- 0.04; t <- 1; q<- 0; k <- 80
p <- function(v) bso(s,k,r,q,v,t)
curve(p, 0, 1, xlab = expression(sigma), ylab = expression(P[t]), ylim = c(0,40))
k <- 100; curve(p, 0, 1, add = T, lty = 2)
k <- 120; curve(p, 0, 1, add = T, lty = 3)      
legend('bottomright', c('k = 80', 'k = 100', 'k = 120'), lty = 1:3, bty = 'n')
# 시간에 따른 옵션 가격 변화
s <- seq(50,150,5); opt <- bso(s,100,0.04,0,0.2,1)
opt
plot(s, opt, bty = 'n', type = 'l', col = 'red')
t.seq <- seq(0,1,0.1)
for (t in t.seq) {
  f <- function(x) bso(x,100,0.04,0,0.2,t)
  curve(f, add = TRUE)
}

## (2) 풋콜옵션을 반영한 블랙숄즈 가격함수

