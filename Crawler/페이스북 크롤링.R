# 셀레늄 패키지와 rvest 패키지를 설치합니다.
source("https://install-github.me/ropensci/RSelenium")
install.packages("rvest")
install.packages("dplyr")

# 패키지를 불러서 사용할 준비를 합니다.
library(RSelenium)
library(rvest)
library(dplyr)

# phantomjs 브라우저를 사용합니다.
pJS <- wdman::phantomjs(port = 4567L)
Sys.sleep(5)
# 컨트롤 드라이버를 연결합니다.
remDr <- remoteDriver(browserName = 'phantomjs', port = 4567L)
# 컨트롤 드라이버를 실행합니다.
remDr$open()
# 기업 페이지를 켭니다. jtbc 를 선택했습니다.
remDr$navigate("https://m.facebook.com/jtbcnews")
# 스크린샷을 확인합니다. 아래에 계속 있는 코드들도 실제 실행시는 없어도 됩니다. 확인용도로만 사용하세요.
# remDr$screenshot(display = T)
Sys.sleep(5)

# 화면을 아래로 계속 내리는 동작을 합니다.
# 그래야 과거 포스트를 계속 불러와줍니다.
# while 문 안에 10을 큰 숫자로 바꾸면 더 많은 포스트를 불러옵니다.
webElem <- remDr$findElement(using = 'css selector', ".scrollArea")
webElem$sendKeysToElement(list(key = "end"))
Sys.sleep(3)
len <- 0

while(len<10){
  webElems <- remDr$findElements(using = 'css selector', ".story_body_container")
  len <- length(webElems)
  print(len)
  remDr$mouseMoveToLocation(webElement = webElems[[1]])
  remDr$buttondown()
  remDr$mouseMoveToLocation(webElement = webElems[[len]])
  remDr$buttonup()
  Sys.sleep(5)
}

# 포스트 단위로 구분해서 가져옵니다.
remDr$getPageSource()[[1]] %>% 
  read_html() %>% 
  html_nodes("article") -> articles

# 공유 컨텐츠를 분리합니다.
containers <-
  lapply(articles, function(article)
    article %>% 
      html_nodes(".story_body_container")
  )

# 포스트 내의 글자만 가져와서 저장합니다.
# 공유 컨텐츠의 처리가 있어 조금 복잡합니다.
posts <- 
  lapply(containers, function(container){
    if(length(container)==1){
      container %>% 
        html_nodes("div span p") %>%
        html_text %>%
        paste0(collapse = " ") %>% 
        c("") -> res
    }
    if(length(container)==2){
      
      c1 <- container[1] %>% 
        html_nodes("div span p") %>%
        html_text %>%
        paste0(collapse = " ")
      c2 <- container[2] %>% 
        html_nodes("div span p") %>%
        html_text %>%
        paste0(collapse = " ")
      res <- c(
        ifelse(c1 != "",c1,""),
        ifelse(c2 != "",c2,"")
      )
      res[1]<- ifelse(c1==c2,"",c1)
    }
    return(data.frame(content = res[1],
                      nested_content = res[2],
                      stringsAsFactors = F))
  })

# data.frame으로 저장합니다.
posts <- bind_rows(posts)

# 좋아요, 댓글, 공유 수 정보를 가져오기 위해 포스트 단위로 구분해 저장합니다.
remDr$getPageSource()[[1]] %>% 
  read_html() %>% 
  html_nodes("footer") -> count_info

# 좋아요 수를 가져옵니다.
like <-
  sapply(count_info, function(x){
    x %>% 
      html_nodes("div div a span.like_def") %>% 
      html_text %>% 
      gsub("[좋아요 |개|,]","",.) %>% 
      as.integer() -> res
    if(identical(res,integer(0))){
      res<-0
    }
    return(res)
  })

# 댓글 수를 가져옵니다.
comment <-
  sapply(count_info, function(x){
    x %>% 
      html_nodes("div div a span.cmt_def") %>% 
      html_text %>% 
      gsub("[댓글 |개|,]","",.) %>% 
      as.integer() -> res
    if(identical(res,integer(0))){
      res<-0
    }
    return(res)
  })  

# 공유 수를 가져옵니다.
share <-
  sapply(count_info, function(x){
    x %>% 
      html_nodes(xpath="div/div/a/span[2]") %>% 
      html_text %>% 
      gsub("[공유 |회|,]","",.) %>% 
      as.integer() -> res
    if(identical(res,integer(0))){
      res<-0
    }
    return(res)
  })  

# 포스트 단위로 저장합니다.
tar <- bind_cols(posts,
                 data.frame(like),
                 data.frame(comment),
                 data.frame(share))

# 컨트롤 드라이버를 종료합니다.
remDr$closeall()
# 브라우저를 종료합니다.
pJS$stop()