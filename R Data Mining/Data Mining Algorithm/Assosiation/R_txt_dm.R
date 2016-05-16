
#设置工程路径
setwd("C:/Users/mhuang1/Documents/GitHub/r_dataanalysis")
dir()

sogou_q <- read.csv("sogou_q.csv",col.names = c("id","search_item","id1","id2","url"))         
head(sogou_q)

