# install.packages("readxl")
# install.packages("xlsx")
# install.packages("knitr")
# install.packages("car")
require(knitr)
require(readxl)
require(car)
MinWage1 <- read_xlsx("MinWage.xlsx", sheet = 1)
MinWage2 <- read_xlsx("MinWage.xlsx", sheet = 2)
MinWage3 <- read_xlsx("MinWage.xlsx", sheet = 3)
MinWage4 <- read_xlsx("MinWage.xlsx", sheet = 4)
MinWage5 <- read_xlsx("MinWage.xlsx", sheet = 5)
MinWage6 <- read_xlsx("MinWage.xlsx", sheet = 6)

ReadAll <- function(xlName, destNum) {
  require(readxl)
  for (i in 1 : destNum) {
    n <- paste0(xlName, i)
    n <- read_xlsx(xlName, sheet = i)
    n <- as.data.frame(n)
    return(n)
  }
}
# ReadAll("MinWage.xlsx", 6)

Group1 <- 0.1 * 0.1 * (5580/15978)
Group2 <- 0.03 * 0.13 * 0.5
Group3 <- 0.01 * 0.27 * 0.7
Group <- Group1 + Group2 + Group3
Group

A_transpose <- as.matrix(MinWage4)
A_hatV <- as.matrix(MinWage5)
P_dotV <- as.matrix(MinWage6) + Group
I <- diag(33)
PriceAffectedCoefficient  <- solve(I - A_transpose)
Solution <- t(P_dotV) %*% (PriceAffectedCoefficient %*% A_hatV)
Solution <- t(Solution)
knitr::kable(Solution)


Group12 <- 0.15 * 0.1 * (5580/15978)
Group22 <- 0.03 * 0.13 * 0.5 * 1.5
Group32 <- 0.01 * 0.27 * 0.7 * 1.5
Group2 <- Group12 + Group22 + Group32
Group2

P_dotV2 <- as.matrix(MinWage6) + Group2
I <- diag(33)
PriceAffectedCoefficient  <- solve(I - A_transpose)
Solution2 <- t(P_dotV2) %*% (PriceAffectedCoefficient %*% A_hatV)
Solution2 <- t(Solution2)
knitr::kable(Solution2)

MinWage1_Adj <- MinWage1[c(2,6,7,9,10,13,14,15,19,23)]

Linear <- lm(`고용률(15-64세)` ~ ., data = MinWage1_Adj)
summary(Linear)
anova(Linear)
car::vif(Linear)
plot(Linear)