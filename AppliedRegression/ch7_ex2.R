## stackloss{datasets} 자료에 대해 저항회귀를 수행한다.
data(stackloss)
force(stackloss)
str(stackloss)
pairs(stackloss)
## LMS와 LTS 회귀 적합
# 일차분석에서 유의하지 않은 변수(Acid.Conc.)는 제외하였음
# LMS 회귀적합: MASS::lmsreg() 사용
MASS::lmsreg(stack.loss ~ . -Acid.Conc. , data = stackloss)
# lqs(stack.loss ~ . - Acid.Conc., data = stackloss, method = "lms")와 동일

# LTS 회귀적합: MASS::ltsreg() 사용
MASS::ltsreg(stack.loss ~ . -Acid.Conc. , data = stackloss)
# lqs(stack.loss ~ . - Acid.Conc., data = stackloss, method = "lts")와 동일

## 일반선형회귀모형과의 비교
glm(stack.loss ~ . -Acid.Conc. , data = stackloss)
