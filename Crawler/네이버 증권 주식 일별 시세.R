
dev.off()

# rm(list=lsf.str()) # 변수 빼고 함수만 지우기
# rm(list=setdiff(ls(), lsf.str())) # 함수 빼고 변수만 지우기

install.packages("rvest"); install.packages("R6");install.packages("dpylr")
install.packages("quantmod");install.packages("tseries");install.packages("fImport")
library(rvest); library(R6); library(dplyr)
library(quantmod);library(tseries);library(fImport)
require(ggplot2)
# 네이버 증권에서 한국전력 확인. 한국전력의 주식코드는 015760
# 일별시세를 나타내는 사이트 확인(우측클릭->링크복사)
# 테이블 주소 확인
pac[1]
library(pac[1])

installPac <- function() {
  pac <- c("ggplot2", "dplyr", "rvest")
  if (length(setdiff(pac, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(pac, rownames(installed.packages()))) 
  }
  for (i in length(pac)) {
    library(pac[i])
  }
}
remove.packages("rvest")
installPac()
# rvest는 html 스크래핑하는 패키지

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
pkg <- c("ggplot2", "dplyr", "rvest")
ipak(pkg)


Initial <- function() {
  rm(list=ls())
  rm(list=lsf.str())
  rm(list=setdiff(ls(), lsf.str()))
  cat("\014")
  setwd("D:/R_LAB")  
}
Initial()


Crawl <- function(company, page) {
  company <- as.character(company)
  basic_url <- paste0('https://finance.naver.com/item/sise_day.nhn?code=',company,'&page=')
  # paste0()은 나열된 원소 사이에 공백을 두지 않고 결과값을 출력하는 기본함수
  all_price <- c()
  
  for (page in 1:page) {
    price <- paste0(basic_url, page) %>% 
      read_html() %>% 
      html_nodes('table') %>% # table이라는 태그만 찾고
      .[1] %>% # 
      html_table() # parsing한 html을 dataframe 형식으로 저장
    
  all_price <- c(all_price, price)
  return(all_price)
  # save(all_price, file = "all_price.RData")
  } 
}
Crawl(015760, 50)

Preprocess <- function(company, page) {
  AP <- all_price[[1]];
  for (i in 2:page) {
    AP <- rbind(AP, all_price[[i]]) # 리스트를 데이터프레임으로 바꾸기
  }
  comNum <- paste0(company,".csv")
  
  write.csv(AP, comNum)
  AP <- read.csv(comNum, header = TRUE, stringsAsFactors = FALSE ,na.strings = c(""))
  AP <- na.omit(AP);AP <- AP %>% select(-1,-4); rownames(AP) <- NULL # 번호, 전일비 삭제(전일비 모두 양수로 나옴)
  
  AP <- sapply(AP, function(AP) gsub(",", "", AP));AP <- as.data.frame(AP)
  write.csv(AP, comNum)
  AP <- read.csv(comNum, header = TRUE, stringsAsFactors = FALSE);AP <- AP %>% select(-1)
  
  AP$날짜 <- as.Date(AP$날짜, format = "%Y.%m.%d")
}
Preprocess(015760, 50)
main <- function(company, page) {

  Crawl(company, page)
  Preprocess(company, page)
  ggplot(data = AP, aes(x = 날짜, y = 종가)) + 
    geom_line() +
    scale_y_continuous(labels = scales::comma)
  
}

main(015760, 50)
main(068270, 50)

company = '068270'
page = 50
basic_url <- paste0('https://finance.naver.com/item/sise_day.nhn?code=',company,'&page=')
all_price <- c()

for (page in 1:page) {
  price <- paste0(basic_url, page) %>% 
    # paste는 나열된 원소 사이에 공백을 두고 결과값을 출력하는 기본함수
    read_html() %>% 
    html_nodes('table') %>% # table이라는 태그만 찾고
    .[1] %>% # 
    html_table() # parsing한 html을 dataframe 형식으로 저장
  
  all_price <- c(all_price, price)
} 


AP <- all_price[[1]];
for (i in 2:page) {
  AP <- rbind(AP, all_price[[i]]) # 리스트를 데이터프레임으로 바꾸기
}

comNum <- paste0(company,".csv")
write.csv(AP, comNum)
AP <- read.csv(comNum, header = TRUE, stringsAsFactors = FALSE ,na.strings = c(""))
AP <- na.omit(AP);AP <- AP %>% select(-1,-4); rownames(AP) <- NULL # 번호, 전일비 삭제(전일비 모두 양수로 나옴)
str(AP);class(AP) # 자료형이 모두 문자라 분석이 곤란, 전체 형태도 데이터 프레임에서 행렬로 바뀜

AP <- sapply(AP, function(AP) gsub(",", "", AP))
AP <- as.data.frame(AP)
write.csv(AP, comNum)
AP <- read.csv(comNum, header = TRUE, stringsAsFactors = FALSE);AP <- AP %>% select(-1)
str(AP);class(AP) # csv로 넘겼다가 다시 오면서 자료형 자동 변환. 날짜만 조정

AP$날짜 <- as.Date(AP$날짜, format = "%Y.%m.%d")
library(ggplot2)
ggplot(data = AP, aes(x = 날짜, y = 종가)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma)
