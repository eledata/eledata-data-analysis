




qn_data <- rnorm(100000, 0 ,1)
qn_data_de <- density(qn_data)
hist(qn_data, freq = FALSE)
lines(qn_data_de, col = "blue")


qn_data <- rnorm(100000, 0 ,1)
stem(qn_data)
boxplot(qn_data)
fivenum(qn_data)
ecdf_data <- ecdf(qn_data)
plot(ecdf_data)



qn_data <- rnorm(1000, 0 ,1)
shapiro.test(qn_data)


 



