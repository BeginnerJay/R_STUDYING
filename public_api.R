library(httr)
library(XML)

serviceURL <- "http://apis.data.go.kr/9760000/PofelcddInfoInqireService/"

operation <- "getPofelcddRegistSttusInfoInqire"

sgid <- 20220601

sgTypecode <- 3

ServiceKey <- "QnmUCPnX9e1SFHIN2kwJMnDtFsD8O4JYSrWimYvWeu3ZGhwtk4BeEpmkueszZvjpMUBwJv56Vos9Zgee/whhGQ=="

pageNo <- 1

numOfRows <- 10

url <- paste0(serviceURL,
              operation,
              paste0("?serviceKey=", ServiceKey),
              paste0("&sgid=", sgid),
              paste0("&sgTypecode=", sgTypecode),
              paste0("&pageNo=", pageNo),
              paste0("&numOfRows=", numOfRows)
              )

# OpenAPI 호출
xmlDocument <- GET(url)

  `xmlDocument <- xmlTreeParse(url, useInternalNodes = TRUE)

View(xmlDocument)
