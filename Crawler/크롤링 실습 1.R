basic_url <- 'http://news.donga.com/search?query=bigkini&more=1&range=3&p='

urls <- NULL

for(x in 1:100) {
  urls[x] <- paste0(basic_url, x)
}
urls

install.packages("rvest"); install.packages("dplyr")
library(rvest)
library(dplyr)

read_html(urls[1]) # read_html 함수로 html을 r에서 읽기
html_1 <- read_html((urls[1]))
html_1

html_2 <- html_nodes(html_1, '.searchCont') # rvest에서 특정 태그를 찾는 함수 html_nodes()
# serachCont앞에 . 붙여준 이유 : 찾고자 하는 게 class라는 것을 알려 주는 기능
# class 찾을 때는 앞에 . id 찾을 때는 앞에 #
html_3 <- html_nodes(html_2, 'a')
html_3

links <- html_attr(html_3, 'href') # 속성을 가져오는 함수를 이용하여 url만 수집ㅜ
links

links <- html_1 %>% # 
  html_nodes('.searchCont') %>% 
  html_nodes('a') %>% 
  html_attr('href') %>% 
  unique() # 중복되는 값 제거

links <- html_1 %>% # 목록 페이지를 읽어서
  html_nodes('.searchCont') %>% # searchCont 클래스 찾고
  html_nodes('a') %>% # 태그 찾아서
  html_attr('href') %>% # URL만!
  unique() # 중복되는 값 제거
links <- links[-grep("pdf", links)]

links <- NULL
for(url in urls){
  html <- read_html(url)
  links <- c(links, html %>% 
               html_nodes('.searchCont') %>%
               html_nodes('a') %>%
               html_attr('href') %>%
               unique()
             )
}

links <- links[-grep("pdf", links)]
length(links)

txts <- NULL
for (link in links) {
  html <- read_html(link)
  txts <- c(txts, html %>% 
              html_nodes('.article_txt') %>%
              html_text) # 텍스트를 긁어올 때에는 html_text
}

getwd()
setwd("D:/R_LAB")

write.csv(txts, "text.csv")


# 출처: https://kuduz.tistory.com/1041 [kini'n creations]














# 출처: https://kuduz.tistory.com/1041 [kini'n creations]