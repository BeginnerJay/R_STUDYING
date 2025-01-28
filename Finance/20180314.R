a <- 1
b <- 3
a + b

# V(0) = 100, R=0.1, T=5 일 때
V0 <- 100
R <- 0.1
T <- 5
n <- 4

VT <- V0*(1+R/n)^(n*T)
VT

CR = 0.1
T <- 1
ER <- exp(R*T) - 1
ER
# 1 + ER = exp(RT), ER = exp(RT) - 1



# 이자 게산방식 별 원리금 추이 비교

# 입력 조건
Mat <- 10 # 만기
Freq <- 4 # 연 이자 지급 횟수
R <- 0.05 # 금리
V0 <- 100 # 원금

# 원리금 계산
t <- seq(from = 0, to = Freq*Mat, by=1/Freq) # 시간 수열
t
VT_comp <- (1+R/Freq)^(t) # 복리
VT_simp <- 1+(R/Freq)*t # 단리

# 그래프
plot(t, VT_comp, type = 'l', col = 1, main = "Total Amount", xlab = "time", ylab = "Amount")
lines(t, VT_simp, lty = 2, col = 2) # 겹쳐 그리기
legend("topleft", c("Compounded", "Simple"), lty = 1:2, col = 1:2)
grid(col = 'gray')


# 기초 사용법 : 벡터 및 행렬 관련
# 벡터 만들기 : c(), seq(), rep() 함수
a = 1 # 스칼라 만들기
a = 