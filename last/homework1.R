rowdata <- read.csv(file = "D:/data/D_DATAFLOW_row_e.csv", header = T)
rowdata$Date <- as.Date(rowdata$Date,format = "%Y-%m-%d")
library(dplyr)
rowdata <- filter(rowdata, Date > 3651) # 1970-01-01 is 1
rowdata$JPY.Japanese.Yen <- rowdata$JPY.Japanese.Yen/100
rowdata$KRW.Korean.Won <- rowdata$KRW.Korean.Won/1000

library(ggplot2)
library(reshape2)

multi_graph_all <- melt(rowdata, id.vars = "Date")
ggplot(multi_graph_all, aes(x = Date, y = value)) +
  geom_line(aes(color = variable), size = 1) + 
  theme_minimal()

multi_graph_1 <- data.frame(rowdata$Date, rowdata$USD.US.Dollar, rowdata$KRW.Korean.Won, rowdata$JPY.Japanese.Yen, rowdata$GBP.Pound.Sterling)
colnames(multi_graph_1) <- c("Date", "USD", "KRW", "JPY", "GBP")
multi_graph_1 <- melt(multi_graph_1, id.vars = "Date")
colnames(multi_graph_1)[2] <- c("Currency")
ggplot(multi_graph_1, aes(x = Date, y = value)) +
  geom_line(aes(color = Currency), size = 1) + 
  scale_color_manual(values = c("#000000","#191970","#720000", "#E7B800"))
  theme_minimal()

multi_graph_2 <- data.frame(rowdata$Date, rowdata$GBP.Pound.Sterling, rowdata$USD.US.Dollar, rowdata$EUR.Euro,rowdata$KRW.Korean.Won ,rowdata$JPY.Japanese.Yen)
multi_graph_2<- melt(multi_graph_2, id.vars = "rowdata.Date")
colnames(multi_graph_2)[1] <- c("Date")
ggplot(multi_graph_2, aes(x = Date, y = value)) +
  geom_line(aes(color = variable), size = 1) + 
  theme_minimal()

