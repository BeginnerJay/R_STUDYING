# 이자부리 방식별 원리금 추이를 비교하는 그래프를 그리는 R 프로그램을 작성하시오. (※파일명: “HW03a_이름.R")
# 연1회 이자지급 복리방식 vs. 연속복리 방식. 만기는 10년 까지.
# “이자율의 이해와 분석” 강의노트의 6쪽 실습 프로그램을 활용.
rm(list=ls())

# 입력 조건

Mat <- 10 # 만기
Freq <- 1 # 연 이자지급 횟수
R <- 0.1 # 금리
V0 <- 100 # 원금

# 원리금 계산
t <- seq(from = 0, to = Freq*Mat, by = 1/Freq) # 시간 수열
t
VT_comp <- (1 + R / Freq) ^ t # 복리
VT_simp <- 1 + (R / Freq) * t # 단리

# 그래프
plot(t, VT_comp, type = 'l', col = 1, main = "Total Amount", xlab = "time", ylab = "Amount")
lines(t, VT_simp, lty = 2, col = 2) # 겹쳐 그리기
legend('topleft', c("Compounded", "Simple"), lty = 1 : 2, col = 1 : 2)
grid(col = 'gray')
