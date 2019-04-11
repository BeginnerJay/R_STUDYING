install.packages("rvest"); install.packages("dplyr")
library(rvest)
library(dplyr)

basic_url <- 'http://news.donga.com/search?query=bigkini&more=1&range=3&p='

urls <- NULL
for(x in 1:100) {
  urls[x] <- paste0(basic_url, x)
}

links <- NULL
for(url in urls){
  html <- read_html(url)
  links <- c(links, html %>% 
               html_nodes('.searchCont') %>%
               # rvest에서 특정 태그를 찾을 때는 html_node(s)를 쓰면 됩니다.
               # s가 붙으면 같은 태그를 전부 찾고, 빼면 맨 처음에 나오는 하나만 찾습니다.
               html_nodes('a') %>%
               html_attr('href') %>%
               unique()
  )
}
links <- links[-grep("pdf", links)]

txts <- NULL
for (link in links) {
  html <- read_html(link)
  txts <- c(txts, html %>% 
              html_nodes('.article_txt') %>%
              html_text) # 텍스트를 긁어올 때에는 html_text
}
txts

getwd()
setwd("D:/R_LAB")

write.csv(txts, "text.csv")

# 출처: https://kuduz.tistory.com/1041 [kini'n creations]