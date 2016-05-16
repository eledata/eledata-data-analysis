df_data <- read.csv("pa12.csv")
id <- unique(df_data$ID)
n <- length(id)
print(n)
for (i in 1:n){
  iddata <- id[i:i]
  id_data <- subset(df_data, df_data$ID == iddata)
  
  # hf month data
  id_name <- unique(id_data[1])
  hf_month_1 <- id_data[1:1,13]
  hf_month_2 <- id_data[1:1,14]
  hf_month_3 <- id_data[1:1,15]
  hf_month_4 <- id_data[1:1,16]
  hf_month_5 <- id_data[1:1,17]
  hf_month_6 <- id_data[1:1,18]
  hf_month_7 <- id_data[1:1,19]
  hf_month_8 <- id_data[1:1,20]
  hf_month_9 <- id_data[1:1,21]
  hf_month_10 <- id_data[1:1,22]
  hf_month_11 <- id_data[1:1,23]
  hf_month_12 <- id_data[1:1,24]
  
  # hd month data
  hd_month_1 <- id_data[2:2,13]
  hd_month_2<- id_data[2:2,14]
  hd_month_3 <- id_data[2:2,15]
  hd_month_4 <- id_data[2:2,16]
  hd_month_5 <- id_data[2:2,17]
  hd_month_6 <- id_data[2:2,18]
  hd_month_7 <- id_data[2:2,19]
  hd_month_8 <- id_data[2:2,20]
  hd_month_9 <- id_data[2:2,21]
  hd_month_10 <- id_data[2:2,22]
  hd_month_11 <- id_data[2:2,23]
  hd_month_12 <- id_data[2:2,24]
  
  newdata <- data.frame(
    hf_month <- c(hf_month_1,hf_month_2,hf_month_3,hf_month_4,hf_month_5,hf_month_6,hf_month_7,hf_month_8,hf_month_9,hf_month_10,hf_month_11,hf_month_12),
    hd_month <- c(hd_month_1,hd_month_2,hd_month_3,hd_month_4,hd_month_5,hd_month_6,hd_month_7,hd_month_8,hd_month_9,hd_month_10,hd_month_11,hd_month_12))
  
  attach(newdata)
  print (hf_month)
  print (hd_month)

}


write.csv(result, "pa12_result.csv", row.names = FALSE)
newdata.mhf_month<-mean(hf_month); newdata.mhf_month
newdata.mhd_month<-mean(hd_month); newdata.mhd_month
newdata.s<-cov(newdata); newdata.s
newdata.r<-cor(newdata); newdata.r

r <- cor.test(hf_month,hd_month)
result <- data.frame(
  id = id_name,
  mhf_month = mean(hf_month),
  mhd_month = mean(hd_month),
  t_data = r[1],
  df_data = r[2],
  p_data = r[3],
  estcor = r[4]
)
print (result)