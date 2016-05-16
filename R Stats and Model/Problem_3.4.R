
p_data <- data.frame(
  x <- c(2,4,3,2,4,7,7,2,2,5,4,3),
  y <- c(5,6,8,5,10,7,12,12,6,6,6,8),
  z <- c(7,11,6,6,7,9,5,5,10,6,3,10)
)
p_data
# Step !: calculate the mean
p_data.mx <- mean(p_data$x)
p_data.mx
p_data.my <- mean(p_data$y)
p_data.my
p_data.mz <- mean(p_data$z)
p_data.mz


# Step 2: Calculate the cov&cor
p_data.v <- cov(p_data)
p_data.v
p_data.r <- cor(p_data)
p_data.r

# Step 3: Using the cor.test to verify the sample's cor and tot's cor
cor.test(~x + y, data = p_data)
cor.test(~x + z, data = p_data)
cor.test(~y + z, data = p_data)



p_data <- data.frame(
  x <- c(2,4,3,2,4,7,7,2,2,5,4,NaN),
  y <- c(5,6,8,5,10,7,12,12,6,6,NaN,NaN),
  z <- c(7,11,6,6,7,9,5,5,10,6,3,10)
)
plot(p_data)
boxplot(p_data)
plot(z)






