# 초기 환경 설정
rm(list=ls())
rm(list=lsf.str())
rm(list=setdiff(ls(), lsf.str()))
dev.off()   
cat("\014")

### 발행일=2017.08.09, 만기=3년, 원금=10,000, 이표율=10%(Quarterly 후급)인 이표채가 2017.08.14에 거래되었다. YTM=12.36%라고 할 때, 거래가격을 Dirty Price로 각각 계산하시오
## dirty price. 경과이자는 단리로 처리
## dirty price = BVLC*(1+(y/m)*(t/T)) or (BVNC + c/m)/((1+(y/m))^((T-t)/T))
# ex) 9,431 (※경과이자 = 10,000×10%/4×5/92 = 14)
# BVLC = (지난 이표 지급일 or 발행일)의 채권의 가치
# BVNC = 다가올 이표 지급일에서의 채권의 가치
# t = 지난 이표 지금일로부터 흐른 시간
# T 이표 지급 기간의 일수

Bond.P <- function(F,T,c,m,y) {
  r = 1+(y/m)
  n = m*T
  p.coupon = F*(c/y)*(1-(1/r^n))
  p.principal = F*(1/r^n)
  ans <- p.coupon + p.principal
  return(ans)
}

BVLC <- Bond.P(10000,3,0.1,4,0.1236)
BVLC.plus <- Bond.P(10000,3,0.1,4,0.1336)
BVLC.minus <- Bond.P(10000,3,0.1,4,0.1136)
BPDP <- BVLC*(1+(0.1236/4)*(5/92))
BPDP.plus <- BVLC.plus*(1+(0.1336/4)*(5/92))
BPDP.minus <- BVLC.minus*(1+(0.1136/4)*(5/92))

BVNC <- Bond.P(10000,2.75,0.1,4,0.1236) + 250
BVDP <- BVNC/(1+(0.1236/4)*(87/92))
BVNC.plus <- Bond.P(10000,2.75,0.1,4,0.1336) + 250
BVDP.plus <- BVNC.plus/(1+(0.1336/4)*(87/92))
BVNC.minus <- Bond.P(10000,2.75,0.1,4,0.1136) + 250
BVDP.minus <- BVNC.minus/(1+(0.1136/4)*(87/92))
AI <- 10000*0.1/4*5/92

# 유효 듀레이션 구하기
ED.plus <- ((BVDP.plus - BVDP)/0.01)*(1/BVDP)
ED.minus <- ((BVDP-BVDP.minus)/0.01)*(1/BVDP)
ED <- (ED.minus+ED.plus)/2

