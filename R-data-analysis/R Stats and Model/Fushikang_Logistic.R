

suicide_time <- c(0,75,272,758,794,950,997,1003,1015,1023,1024,1024,1053,1051,1072)
suicide_number <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
suicide_data <- data.frame(time = suicide_time, number = suicide_number)
glm.sol<-glm(number~time, family = binomial, data = suicide_data)
summary(glm.sol)
d <- seq(0, 3000, len = 1)
pre <- predict(glm.sol, data.frame(x = d))
p <- exp(pre)/(1+exp(pre))
plot(suicide_data)
lines(d,p)
