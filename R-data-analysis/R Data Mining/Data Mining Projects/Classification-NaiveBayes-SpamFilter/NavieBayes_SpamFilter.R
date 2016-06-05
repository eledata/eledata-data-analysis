# 多个决策边界进行分类

# Data
require(ggplot2)
x <- runif(1000, 0, 40)
y1 <- cbind(runif(100, 0, 10), 1)
y2 <- cbind(runif(800, 10, 30), 2)
y3 <- cbind(runif(100, 30, 40), 1)
value <- data.frame(cbind(x, rbind(y1, y2, y3)), stringsAsFactors = TRUE) # 表明字符可转因子

# 用ggplot2画图
hline <- data.frame(hl = c(10, 30)) # 用于分割geom_hline
mutil <- ggplot(value, aes(x, V2)) +
  geom_jitter(aes(shape = as.factor(V3)), height = 2) +
  scale_shape_discrete(guide = "none", solid = FALSE) +
  geom_hline(aes(yintercept = hl), linetype = 3, hline) +
  theme_bw() +
  xlab("X") +
  ylab("Y")
mutil


# 垃圾邮件分类
require(tm)

# 设置数据路径
spam.path <- file.path("data", "spam")
spam2.path <- file.path("data", "spam_2")
easyham.path <- file.path("data", "easy_ham")
easyham2.path <- file.path("data", "easy_ham_2")
hardham.path <- file.path("data", "hard_ham")
hardham2.path <- file.path("data", "hard_ham_2")

# 读取邮件正文, path 表示文件路径和文件名称 /spam/text.txt, 返回文本向量
get.msg <- function(path){
  open_file <- file(path, open = "rt", encoding = "latin1")
  mail_text <- readLines(open_file) # 读完文件之后，返回向量化文本，mail_text[1] 表示第一行内容
  mt_start <- as.numeric(which(mail_text == "")[1] + 1)
  mt_len <- as.numeric(length(mail_text))
  if(mt_start > mt_len) mt_start = mt_len - 1
  mail_content <- mail_text[seq(mt_start, mt_len, 1)]
  close(open_file)
  return(paste(mail_content, collapse = "\n"))
}

# 构造词项-文档矩阵 TDM [i,j], 词项i在文档j中出现的次数
get.tdm <- function(doc.vec){
  doc.corpus <- Corpus(VectorSource(doc.vec)) # 语料库
  control <- list(stopwords = TRUE, removePunctuation = TRUE, removeNumbers = TRUE)
  doc.tdm <- TermDocumentMatrix(doc.corpus, control) #计算词项-文档矩阵 TDM， 
  return(doc.tdm)
}

# 查询词项出现个数
count.word <- function(doc.vec, term){
  tdm <- get.tdm(doc.vec)
  word.freq <- rowSums(as.matrix(tdm))
  term.freq <- word.freq[which(names(word.freq) == term)]
  return (ifelse(length(term.freq) > 0, term.freq, 0))
}

# 检验垃圾邮件
classify.email <- function(doc.vec, train.df, prior = 0.5, c = 1e-6){
  tdm <- get.tdm(doc.vec)
  msg.freq <- rowSums(as.matrix(tdm))
  msg.match <- intersect(names(msg.freq), train.df$term)

  if(length(msg.match) < 1)
  {
    return(prior * c ^ (length(msg.freq)))
  }
  else
  {
    # 利用贝叶斯定理来求解
    match.probs <- train.df$occurrence[match(msg.match, train.df$term)]
    return(prior * prod(match.probs) * c ^ (length(msg.freq) - length(msg.match)))
  }
}


# 1. Spam 文本向量
spam.docs <- dir(spam.path)
spam.docs <- spam.docs[which(spam.docs != "cmds")]
all.spam <- sapply(spam.docs,
                   function(p) get.msg(file.path(spam.path, p)))
# 邮件正文提取完毕
head(all.spam)
html.spam <- count.word(all.spam, "html")
table.spam <- count.word(all.spam, "table")
spam.init <- cbind(html.spam, table.spam, "SPAM")

# 信息整理
spam.tdm <- get.tdm(all.spam) # 1. 词项-文档矩阵
spam.tdm.mat <- as.matrix(spam.tdm) # 2. 矩阵化
word.freq <- rowSums(spam.tdm.mat) 
spam.df <- data.frame(cbind(names(word.freq), as.numeric(word.freq)), stringsAsFactors = FALSE)
head(spam.df)
names(spam.df) <- c("term", "frequency") # 3. 添加名称
spam.df$frequency <- as.numeric(spam.df$frequency)
spam.occurrence <- sapply(1:nrow(spam.tdm.mat),
                          function(i)
                          {
                            length(which(spam.tdm.mat[i, ] > 0)) / ncol(spam.tdm.mat)
                          })
head(spam.occurrence)
spam.density <- spam.df$frequency/sum(spam.df$frequency)
spam.df <- transform(spam.df, density = spam.density, occurrence = spam.occurrence)

head(spam.df[with(spam.df, order(-occurrence)),])

# 2. easyham 文本向量
easyham.docs <- dir(easyham.path)
easyham.docs <- easyham.docs[which(easyham.docs != "cmds")]
all.easyham <- sapply(easyham.docs,
                   function(p) get.msg(file.path(easyham.path, p)))
# 邮件正文提取完毕
head(all.easyham)
html.easyham <- count.word(all.easyham, "html")
table.easyham <- count.word(all.easyham, "table")
easyham.init <- cbind(html.easyham, table.easyham, "EASYHAM")
# 信息整理
easyham.tdm <- get.tdm(all.easyham) # 1. 词项-文档矩阵
easyham.tdm.mat <- as.matrix(easyham.tdm) # 2. 矩阵化
easyham.word.freq <- rowSums(easyham.tdm.mat) 
easyham.df <- data.frame(cbind(names(easyham.word.freq), as.numeric(easyham.word.freq)), stringsAsFactors = FALSE)
head(easyham.df)
names(easyham.df) <- c("term", "frequency") # 3. 添加名称
easyham.df$frequency <- as.numeric(easyham.df$frequency)
easyham.occurrence <- sapply(1:nrow(easyham.tdm.mat),
                          function(i)
                          {
                            length(which(easyham.tdm.mat[i, ] > 0)) / ncol(easyham.tdm.mat)
                          })
head(easyham.occurrence)
easyham.density <- easyham.df$frequency/sum(easyham.df$frequency)
easyham.df <- transform(spam.df, density = easyham.density, occurrence = easyham.occurrence)

head(easyham.df[with(easyham.df, order(-occurrence)),])


# 画图
init.df <- data.frame(rbind(spam.init, easyham.init),
                      stringsAsFactors = FALSE)
names(init.df) <- c("html", "table", "type")
init.df$html <- as.numeric(init.df$html)
init.df$table <- as.numeric(init.df$table)
init.df$type <- as.factor(init.df$type)

# 3. 测试
hardham.docs <- dir(hardham.path)
hardham.docs <- hardham.docs[which(hardham.docs != "cmds")]
all.hardham <- sapply(hardham.docs,
                      function(p) get.msg(file.path(hardham.path, p)))
# 邮件正文提取完毕
head(all.hardham)

# use classify function for testing
hardham.spamtest <- classify.email(all.hardham, train.df = spam.df)
hardham.easyhamtest <- classify.email(all.hardham, train.df = easyham.df)

hardham.res <- ifelse(hardham.spamtest > hardham.easyhamtest,
                      TRUE,
                      FALSE)
summary(hardham.res)

doc.vec <- all.hardham
train.df <- spam.df
prior <- 0.5
c <- 0.001
tdm <- get.tdm(doc.vec)
msg.freq <- rowSums(as.matrix(tdm))
msg.match <- intersect(names(msg.freq), train.df$term)

if(length(msg.match) > 0){
  prop <- prior*c^(length(msg.freq))
}else{
  # 利用贝叶斯定理来求解
  match.probs <- train.df$occurrence[match(msg.match, train.df$term)]
  prop <- prior * prod(match.probs) * c ^ (length(msg.freq) - length(msg.match))
}

prop

spam.classifier <- function(path)
{
  pr.spam <- classify.email(path, spam.df)
  pr.ham <- classify.email(path, easyham.df)
  return(c(pr.spam, pr.ham, ifelse(pr.spam > pr.ham, 1, 0)))
}











