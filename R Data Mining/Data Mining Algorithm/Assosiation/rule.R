library(arules)
sohist <- read.transactions("export.csv",sep = ",", format = "basket")
summary(sohist)
sohistsize <- size(sohist)
sohistsize
summary(sohistsize)
sum(sohistsize)
itemFreq <- itemFrequency(sohist)
itemFreq[1:5]
sum(itemFreq)
itemcount <- (itemFreq/sum(itemFreq))*sum(sohistsize)
itemcount
itemFrequencyPlot(sohist, support = 0.1)

sorule <- apriori(sohist, parameter = list(support = 0.01, confidence = 0.8))
write(sorule, file = "sorule.csv", sep = ",", quote = TRUE, row.names = FALSE)
summary(sorule)

inspect(sorule[1:5])

qualityMeasures <- interestMeasure(sorule, method=c("coverage","fishersExactTest","conviction", "chiSquared"), transactions=sohist)  
summary(qualityMeasures)
quality(sorule) <- cbind(quality(sorule), qualityMeasures)  
inspect(head(sort(sorule, by = "conviction", decreasing = F)))  
