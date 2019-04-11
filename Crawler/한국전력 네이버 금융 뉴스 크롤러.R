rm(list=ls())
rm(list=lsf.str())
rm(list=setdiff(ls(), lsf.str()))
dev.off()   
cat("\014")
getwd()
setwd("D:/R_LAB")
getwd()

install.packages("rvest"); install.packages("R6");install.packages("dpylr")
library(rvest); library(R6); library(dplyr)

basic_url <- 'https://finance.naver.com/item/news_news.nhn?code=015760&page='

urls <- NULL
j <- 104
for(x in 1:j) {
  urls[x] <- paste0(basic_url, x)
}
# 여기까지 OK



links <- NULL
for(url in urls){
  html <- read_html(url)
  links <- c(links, html %>% 
               html_nodes('.tb_cont') %>%
               # rvest에서 특정 태그를 찾을 때는 html_node(s)를 쓰면 됩니다.
               # s가 붙으면 같은 태그를 전부 찾고, 빼면 맨 처음에 나오는 하나만 찾습니다.
               html_nodes('a') %>%
               html_attr('href')
  )
  links <- unique(links)
}

View(links)
links <- links[-grep("pdf", links)]

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