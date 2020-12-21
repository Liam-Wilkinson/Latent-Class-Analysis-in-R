####Jaccard Matrix
attach(ProjectData)
ProjectData[is.na(ProjectData)]<-0
A<-matrix(c(X),byrow=T,nrow=12408,ncol=16)
colnames(A)<-letters[1:16]
rownames(A)<-LETTERS[1:12408]
sim.jac<-matrix(0,nrow=nrow(A),ncol=nrow(A))
pairs<-t(combn(1:nrow(A),2))
for(i in 1:nrow(pairs)) {
  num<-sum(sapply(1:ncol(A),function(x)(min(A[pairs[i,1],x],A[pairs[i,2],x]))))
  den<-sum(sapply(1:ncol(A),function(x)(max(A[pairs[i,1],x],A[pairs[i,2],x]))))
  sim.jac[pairs[i,1],pairs[i,2]]<- num/den
  sim.jac[pairs[i,2],pairs[i,1]]<-num/den
}
sim.jac[which(is.na(sim.jac))]<-0
diag(sim.jac)<- 1
dist.jac<-as.dist(1-sim.jac)
hc<-hclust(dist.jac,method="ward.D2")
cut<-as.data.frame(cutree(hc,k=4)
               
fviz_nbclust(X,FUN=hcut,distance="jaccard",method="silhouette")