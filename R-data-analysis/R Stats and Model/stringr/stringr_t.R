library(stringr)

colname <- c("ID","S_Item","Priority","Link","Priority1","Priority2")
sample <- read.csv("sample1.txt",sep = "\t",col.names = colname)
priority <- str_split(sample[,3],"    ")
head(sample[,3])
unlist_priority <- unlist(priority)
head(unlist_priority)

mc <- matrix(unlist_priority, ncol = 2, byrow = T)
sample[5] <- mc[,1]
sample[6] <- mc[,2]
head(sample)
write.csv(sample, file = "finished1.txt")


library(stringr)

colname <- c("ID","S_Item","Priority","Link")
sample <- read.csv("sample1.txt",sep = "\t",col.names = colname)
head(sample)
write.csv(sample[1], file = "finished1.txt")


context <- read.table("sample1.txt",sep = "\t")
head(context)

