distinguish.distance<-function
   (TrnX, TrnG, TstX = NULL, var.equal = FALSE){
   if ( is.factor(TrnG) == FALSE){
       mx<-nrow(TrnX); mg<-nrow(TrnG)
       TrnX<-rbind(TrnX, TrnG)
       TrnG<-factor(rep(1:2, c(mx, mg)))
   }
   if (is.null(TstX) == TRUE) TstX<-TrnX
   if (is.vector(TstX) == TRUE)  TstX<-t(as.matrix(TstX))
   else if (is.matrix(TstX) != TRUE)
      TstX<-as.matrix(TstX)
   if (is.matrix(TrnX) != TRUE) TrnX<-as.matrix(TrnX)

   nx<-nrow(TstX)
   blong<-matrix(rep(0, nx), nrow=1, dimnames=list("blong", 1:nx))
   g<-length(levels(TrnG))
   mu<-matrix(0, nrow=g, ncol=ncol(TrnX))
   for (i in 1:g)
      mu[i,]<-colMeans(TrnX[TrnG==i,]) 
   D<-matrix(0, nrow=g, ncol=nx)
   if (var.equal == TRUE  || var.equal == T){
      for (i in 1:g)
         D[i,]<- mahalanobis(TstX, mu[i,], var(TrnX)) # 比较测试数据与各个因子之间的马氏距离。
   }
   else{
      for (i in 1:g)
         D[i,]<- mahalanobis(TstX, mu[i,], var(TrnX[TrnG==i,]))
   }
   for (j in 1:nx){
      dmin<-Inf
      for (i in 1:g)
          if (D[i,j]<dmin){ # 在多个因子之间，比较最小的马氏距离，然后填入最小马氏距离的i值。相当于一个分发的作用。
             dmin<-D[i,j]; blong[j]<-i
      }
   }
   blong
}


TrnG <- gl(3,50)
TrnX <- iris[,1:4]
g<-length(levels(TrnG))
nx <- nrow(TrnX)
mu<-matrix(0, nrow=g, ncol=ncol(TrnX))
D<-matrix(0, nrow=3, ncol=nx)
mu<-matrix(0, nrow=g, ncol=ncol(TrnX))
for (i in 1:g)
  mu[i,]<-colMeans(TrnX[TrnG==i,]) 


for (i in 1:g)
  D[i,]<- mahalanobis(TrnX, mu[i,], var(TrnX)) #各个属性，到因子1,2,3的距离情况
