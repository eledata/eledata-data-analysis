# 7 个人评价 6 部电影的得分矩阵,最好 5 分，最差 1 分，没有看过记为 NA，表示不评价。
scores = c(2.5,3.5,3,3.5,2.5,3,3,3.5,1.5,5,3.5,3,2.5,3,NA,3.5,NA,4,NA,
           3.5,3,4,2.5,4.5,3,4,2,3,2,3,3,4,NA,5,3.5,3,NA,4.5,NA,4,1,NA)
data = matrix(scores,byrow=T,ncol=6)
colnames(data) = c('张三丰','少林寺','让子弹飞','神雕侠侣','白蛇传','龙门客栈')
rownames(data) = c('赵一','钱二','孙三','李四','刘五','郑六','王七')
# 缺失值用均值替代并作降维处理
view = apply(data,2,function(x){
    x[is.na(x)] = mean(x,na.rm=T)
    return(x)
})
res = svd(view)
# 测量品味接近程度
#install.packages("proxy")
mute = function(x,dim=3){
    u = data.frame(res$u[,1:dim])
    rownames(u) = rownames(data)
    library(proxy)
    similar = as.matrix(simil(u,method='cosine'))
    p = similar[x,]
    res = p[order(p,decreasing=T)]
    return(res)
}

#找出关系网
m = lapply(rownames(data),mute)
relation = data.frame(客户群 = rownames(data),
                         推荐人 = sapply(1:nrow(data),function(i) names(m[[i]][1])),
                         密 切 程 度 = round(as.vector(sapply(1:nrow(data),function(i)
                             m[[i]][1])),2))

# 推荐电影
recommend = function(x){
    part = colnames(data)[is.na(data[x,])]
    if (length(part)==0) return(NA)
    m = mute(x)
    md = data[names(m),part,drop = F]
    movie = colSums(md*m,na.rm = T)/apply(!is.na(md),2,function(x) sum(m[x]))
    names(movie) = part
    res = movie[order(movie,decreasing=T)]
    return(res)
}
# 策略可视化
library(ggplot2)
u = data.frame(res$u[,2:3])
v = data.frame(res$v[,2:3])
p = ggplot() +
    geom_point(data=u,aes(X1,X2),size = 3,colour = 'blue') +
    geom_point(data=v,aes(X1,X2),size=4,colour='red4') +
    geom_text(data=u,aes(X1,X2),label=rownames(data),vjust=2) +
    geom_text(data=v,aes(X1,X2),label=colnames(data),vjust=-1) +
    geom_segment(aes(x = u[4,1], y = u[4,2], xend = u[3,1], yend = u[3,2]),size = 1) +
    geom_segment(aes(x = u[4,1], y = u[4,2], xend = v[1,1], yend = v[1,2]),size = 1) +
    geom_segment(aes(x = u[3,1], y = u[3,2], xend = v[3,1], yend = v[3,2]),size = 1) +
    geom_segment(aes(x = u[7,1], y = u[7,2], xend = u[5,1], yend = u[5,2]),colour = 'red',size = 1) +
    geom_segment(aes(x = u[7,1], y = u[7,2], xend = v[1,1], yend = v[1,2]),colour = 'red',size = 1) +
    geom_segment(aes(x = u[6,1], y = u[6,2], xend = u[2,1], yend = u[2,2]),colour = 'blue',size = 1) +
    geom_segment(aes(x = u[6,1], y = u[6,2], xend = v[3,1], yend = v[3,2]),colour = 'blue',size = 1) +
    coord_cartesian(ylim=c(-0.7,0.7))
print(p)
