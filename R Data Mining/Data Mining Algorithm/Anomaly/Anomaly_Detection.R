
devtools::install_github("petermeissner/wikipediatrend")
devtools::install_github("twitter/AnomalyDetection")
install.packages("Rcpp")

library(wikipediatrend) ## Library containing API wikipedia access
library(AnomalyDetection)
library(ggplot2)

fifa_data = wp_trend("fifa", from = "2013-03-18", to = "2015-09-01", lang = "en")
fifa_data$date = as.POSIXct(fifa_data$date)
fifa_data = fifa_data[,c(1,2)]
fifa_data
ggplot(fifa_data, aes(x = date, y = count, color = count)) + geom_line()


data_anomaly = AnomalyDetectionTs(fifa_data,max_anoms=0.01,direction="pos",plot=TRUE,e_value = T)
data_anomaly
summary(data_anomaly)
data_anomaly$plot






