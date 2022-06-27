library(readxl)
library(dplyr)
library(writexl)

getwd()
setwd("D:/Github/R_Studying")

#########

Met <- read_excel("Metropolitan.xlsx")
str(Met)
DatMet <- as.data.frame(Met)
str(DatMet)
AreaValue <- unique(DatMet[,1])
WayValue <- unique(DatMet[,2])

#########


# 지역 비례 합쳐서 연령별

a <- c(); A <- c() # 20
b <- c(); B <- c() # 30
c <- c(); C <- c() # 40
d <- c(); D <- c() # 50
e <- c(); E <- c() # 60

for(i in 1:length(AreaValue)) {
  a[i] <- filter(DatMet, Area == AreaValue[i] , Age<30) %>% summarise(mean(Wealth))
  A[i] <- a[[i]]
  b[i] <- filter(DatMet, Area == AreaValue[i] , 29<Age & Age<40) %>% summarise(mean(Wealth))
  B[i] <- b[[i]]
  c[i] <- filter(DatMet, Area == AreaValue[i] , 39<Age & Age<50) %>% summarise(mean(Wealth))
  C[i] <- c[[i]]
  d[i] <- filter(DatMet, Area == AreaValue[i] , 49<Age & Age<60) %>% summarise(mean(Wealth))
  D[i] <- d[[i]]
  e[i] <- filter(DatMet, Area == AreaValue[i] , 59<Age) %>% summarise(mean(Wealth))
  E[i] <- e[[i]]
}

Result <- (data.frame(A, B, C, D, E))
names(Result) <- c("20대", "30대", "40대", "50대", "60대 이상")
Result <- Result %>% mutate(Area = AreaValue)
writexl::write_xlsx(Result, "광역지역비례.xlsx")


#비례대표만

a <- c(); A <- c() # 20
b <- c(); B <- c() # 30
c <- c(); C <- c() # 40
d <- c(); D <- c() # 50
e <- c(); E <- c() # 60

for(i in 1:length(AreaValue)) {
  a[i] <- filter(DatMet, Area == AreaValue[i] & Way == WayValue[1], Age<30) %>% summarise(mean(Wealth))
  A[i] <- a[[i]]
  b[i] <- filter(DatMet, Area == AreaValue[i] & Way == WayValue[1], 29<Age & Age<40) %>% summarise(mean(Wealth))
  B[i] <- b[[i]]
  c[i] <- filter(DatMet, Area == AreaValue[i] & Way == WayValue[1], 39<Age & Age<50) %>% summarise(mean(Wealth))
  C[i] <- c[[i]]
  d[i] <- filter(DatMet, Area == AreaValue[i] & Way == WayValue[1], 49<Age & Age<60) %>% summarise(mean(Wealth))
  D[i] <- d[[i]]
  e[i] <- filter(DatMet, Area == AreaValue[i] & Way == WayValue[1], 59<Age) %>% summarise(mean(Wealth))
  E[i] <- e[[i]]
}
rm(a,b,c,d,e)
Result <- (data.frame(A, B, C, D, E))
names(Result) <- c("20대", "30대", "40대", "50대", "60대 이상")
Result <- Result %>% mutate(Area = AreaValue)
writexl::write_xlsx(Result, "광역비례.xlsx")

#지역구만

a <- c(); A <- c() # 20
b <- c(); B <- c() # 30
c <- c(); C <- c() # 40
d <- c(); D <- c() # 50
e <- c(); E <- c() # 60

for(i in 1:length(AreaValue)) {
  a[i] <- filter(DatMet, Area == AreaValue[i] & Way == WayValue[2], Age<30) %>% summarise(mean(Wealth))
  A[i] <- a[[i]]
  b[i] <- filter(DatMet, Area == AreaValue[i] & Way == WayValue[2], 29<Age & Age<40) %>% summarise(mean(Wealth))
  B[i] <- b[[i]]
  c[i] <- filter(DatMet, Area == AreaValue[i] & Way == WayValue[2], 39<Age & Age<50) %>% summarise(mean(Wealth))
  C[i] <- c[[i]]
  d[i] <- filter(DatMet, Area == AreaValue[i] & Way == WayValue[2], 49<Age & Age<60) %>% summarise(mean(Wealth))
  D[i] <- d[[i]]
  e[i] <- filter(DatMet, Area == AreaValue[i] & Way == WayValue[2], 59<Age) %>% summarise(mean(Wealth))
  E[i] <- e[[i]]
}
Result <- (data.frame(A, B, C, D, E))
names(Result) <- c("20대", "30대", "40대", "50대", "60대 이상")
Result <- Result %>% mutate(Area = AreaValue)
writexl::write_xlsx(Result, "광역지역.xlsx")


########################################################




Met <- read_excel("County.xlsx")
str(Met)
DatMet <- as.data.frame(Met)
str(DatMet)
AreaValue <- unique(DatMet[,1])
WayValue <- unique(DatMet[,2])

#########


# 지역 비례 합쳐서 연령별

a <- c(); A <- c() # 20
b <- c(); B <- c() # 30
c <- c(); C <- c() # 40
d <- c(); D <- c() # 50
e <- c(); E <- c() # 60

for(i in 1:length(AreaValue)) {
  a[i] <- filter(DatMet, Area == AreaValue[i] , Age<30) %>% summarise(mean(Wealth))
  A[i] <- a[[i]]
  b[i] <- filter(DatMet, Area == AreaValue[i] , 29<Age & Age<40) %>% summarise(mean(Wealth))
  B[i] <- b[[i]]
  c[i] <- filter(DatMet, Area == AreaValue[i] , 39<Age & Age<50) %>% summarise(mean(Wealth))
  C[i] <- c[[i]]
  d[i] <- filter(DatMet, Area == AreaValue[i] , 49<Age & Age<60) %>% summarise(mean(Wealth))
  D[i] <- d[[i]]
  e[i] <- filter(DatMet, Area == AreaValue[i] , 59<Age) %>% summarise(mean(Wealth))
  E[i] <- e[[i]]
}
rm(a,b,c,d,e,i)
Result <- (data.frame(A, B, C, D, E))
names(Result) <- c("20대", "30대", "40대", "50대", "60대 이상")
Result <- Result %>% mutate(Area = AreaValue)
writexl::write_xlsx(Result, "기초지역비례.xlsx")


#비례대표만

a <- c(); A <- c() # 20
b <- c(); B <- c() # 30
c <- c(); C <- c() # 40
d <- c(); D <- c() # 50
e <- c(); E <- c() # 60

for(i in 1:length(AreaValue)) {
  a[i] <- filter(DatMet, Area == AreaValue[i] & Way == WayValue[1], Age<30) %>% summarise(mean(Wealth))
  A[i] <- a[[i]]
  b[i] <- filter(DatMet, Area == AreaValue[i] & Way == WayValue[1], 29<Age & Age<40) %>% summarise(mean(Wealth))
  B[i] <- b[[i]]
  c[i] <- filter(DatMet, Area == AreaValue[i] & Way == WayValue[1], 39<Age & Age<50) %>% summarise(mean(Wealth))
  C[i] <- c[[i]]
  d[i] <- filter(DatMet, Area == AreaValue[i] & Way == WayValue[1], 49<Age & Age<60) %>% summarise(mean(Wealth))
  D[i] <- d[[i]]
  e[i] <- filter(DatMet, Area == AreaValue[i] & Way == WayValue[1], 59<Age) %>% summarise(mean(Wealth))
  E[i] <- e[[i]]
}
rm(a,b,c,d,e,i)
Result <- (data.frame(A, B, C, D, E))
names(Result) <- c("20대", "30대", "40대", "50대", "60대 이상")
Result <- Result %>% mutate(Area = AreaValue)
writexl::write_xlsx(Result, "기초비례.xlsx")

#지역구만

a <- c(); A <- c() # 20
b <- c(); B <- c() # 30
c <- c(); C <- c() # 40
d <- c(); D <- c() # 50
e <- c(); E <- c() # 60

for(i in 1:length(AreaValue)) {
  a[i] <- filter(DatMet, Area == AreaValue[i] & Way == WayValue[2], Age<30) %>% summarise(mean(Wealth))
  A[i] <- a[[i]]
  b[i] <- filter(DatMet, Area == AreaValue[i] & Way == WayValue[2], 29<Age & Age<40) %>% summarise(mean(Wealth))
  B[i] <- b[[i]]
  c[i] <- filter(DatMet, Area == AreaValue[i] & Way == WayValue[2], 39<Age & Age<50) %>% summarise(mean(Wealth))
  C[i] <- c[[i]]
  d[i] <- filter(DatMet, Area == AreaValue[i] & Way == WayValue[2], 49<Age & Age<60) %>% summarise(mean(Wealth))
  D[i] <- d[[i]]
  e[i] <- filter(DatMet, Area == AreaValue[i] & Way == WayValue[2], 59<Age) %>% summarise(mean(Wealth))
  E[i] <- e[[i]]
}
Result <- (data.frame(A, B, C, D, E))
names(Result) <- c("20대", "30대", "40대", "50대", "60대 이상")
Result <- Result %>% mutate(Area = AreaValue)
writexl::write_xlsx(Result, "기초지역.xlsx")


