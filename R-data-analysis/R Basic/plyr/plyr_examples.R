# Plyr 包使用代码集
# 1. Basic Strategy: split - apply -combine
# Split up a big dataset
# Apply a function to each piece
# Combine all the pieces back together


# 1. US Baby Data Set
library("plyr")
usbaby <- read.csv("bnames.csv", stringsAsFactors = FALSE)

# 查看头尾
head(usbaby, 15)
tail(usbaby, 15)

# 1.1 transform 和summarise
# transform: 修改已有的数据框
# summarise: 创建新的数据框

# 取name中的一个字母，R里面字符计算中1开始
letter <- function(x, n = 1){
  if(n < 0){
    len <- nchar(x)
    n <- len + n + 1
  }
  tolower(substr(x, n, n)) # 从n取到n
}

# name元音字母 aeiou的个数
vowels <- function(x){
  nchar(gsub("[^aeiou]","",x))
}

# first, last, length, vowels, 会附在原始数据列之后
bbnames <- transform(usbaby, 
                     first = letter(name, 1), 
                     last = letter(name, -1), 
                     length = nchar(name),
                     vowels = vowels(name)
                     )
head(bbnames)

# 产生新的计算的结果
summarise(usbaby, 
          max_perc = max(percent),
          min_perc = min(percent)
          )

(one <- subset(usbaby, sex == "boy" & year == 2008))
(sum.boy.2008 <- sum(subset(usbaby, sex == "boy" & year == 2008, year))/2008)
head(rank.one <- transform(one, rank = rank(-percent, ties.method = "first"))) # 引入rank函数

# 2.2 ddply 使用实例
# 2.2.1 
# Area_1
# ddply(data, split way, function apply to each piece, 2nd argument to transform)
head(ddply.rank.one <- ddply(usbaby, c("sex", "year"), transform, rank = rank(-percent, ties.method = "first")))
# Area_2 --> Area_2 == Area_1
pieces <- split(usbaby, list(usbaby$sex,usbaby$year)) #根据sex，year来切片, 切片的作用在，可以按列值来分
len <- length(piece)
results <- vector("list", len)
for(i in seq_along(pieces)){
  piece <- pieces[[i]] # 代表一块切片
  piece <- transform(piece, rank = rank(-percent, ties.method = "first"))
  results[[i]] <- piece
}

result <- do.call("rbind", results)

# 2.2.2
head(ddply(usbaby, c("name"), summarise, tot = sum(percent)))


fl <- ddply(usbaby, c("year", "sex"), 
            summarise, tot = sum(percent))
library(ggplot2)
qplot(year, tot, data = fl, geom = "line", 
      colour = sex, facets = ~ sex)

# 挑战的题目
top100 <- ddply(usbaby, c("name", "year"), transform, tot = sum(percent))










