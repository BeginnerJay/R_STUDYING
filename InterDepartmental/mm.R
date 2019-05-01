# 초기 환경 설정
rm(list=ls())
rm(list=lsf.str())
rm(list=setdiff(ls(), lsf.str()))
dev.off()   
cat("\014")
getwd()
setwd("D:/R_LAB/InterDepartmental")

library(readxl)
First2018 <- read_excel("2018년 1분기.xlsx")
Second2018 <- read_excel("2018년 2분기.xlsx")
Third2018 <- read_excel("2018년 3분기.xlsx")

library(dplyr)
OTT2018 <- bind_rows(First2018, Second2018, Third2018)
table(OTT2018$지역);table(OTT2018$주소)

OTT2018_SJ <- OTT2018 %>% 
  filter(지역 == "서울 중구") %>% 
  select(지역, 측정일시, PM10, PM25, 주소)

SJ_PM10_mean <- mean(OTT2018_SJ$PM10, na.rm = T)
OTT2018_SJ$PM10 <- ifelse(is.na(OTT2018_SJ$PM10),
                          SJ_PM10_mean,
                          OTT2018_SJ$PM10)
SJ_PM25_mean <- mean(OTT2018_SJ$PM25, na.rm = T)
OTT2018_SJ$PM25 <- ifelse(is.na(OTT2018_SJ$PM25),
                          SJ_PM25_mean,
                          OTT2018_SJ$PM25)
table(is.na(OTT2018_SJ))


OTT2018_IG <- OTT2018 %>% 
  filter(주소 == "인천 강화군 송해면 전망대로 29(솔정리)") %>% 
  select(지역, 측정일시, PM10, PM25, 주소)

IG_PM10_mean <- mean(OTT2018_IG$PM10, na.rm = T)
OTT2018_IG$PM10 <- ifelse(is.na(OTT2018_IG$PM10),
                          IG_PM10_mean,
                          OTT2018_IG$PM10)
IG_PM25_mean <- mean(OTT2018_IG$PM25, na.rm = T)
OTT2018_IG$PM25 <- ifelse(is.na(OTT2018_IG$PM25),
                          IG_PM25_mean,
                          OTT2018_IG$PM25)
table(is.na(OTT2018_IG))


OTT2018_GC <- OTT2018 %>% 
  filter(주소 == "강원 춘천시 중앙로길 135(중앙로 3가 67-1)") %>% 
  select(지역, 측정일시, PM10, PM25, 주소)

GC_PM10_mean <- mean(OTT2018_GC$PM10, na.rm = T)
OTT2018_GC$PM10 <- ifelse(is.na(OTT2018_GC$PM10),
                          GC_PM10_mean,
                          OTT2018_GC$PM10)
GC_PM25_mean <- mean(OTT2018_GC$PM25, na.rm = T)
OTT2018_GC$PM25 <- ifelse(is.na(OTT2018_GC$PM25),
                          GC_PM25_mean,
                          OTT2018_GC$PM25)
table(is.na(OTT2018_GC))


OTT2018_temp <- left_join(OTT2018_SJ, OTT2018_IG, by = "측정일시")
OTT2018_Anal <- left_join(OTT2018_temp, OTT2018_GC, by = "측정일시")
rm(OTT2018_temp, First2018, Second2018, Third2018)

write.csv(
  OTT2018_Anal,
  file = "OTT2018_Anal.csv",
  row.names = TRUE
)
