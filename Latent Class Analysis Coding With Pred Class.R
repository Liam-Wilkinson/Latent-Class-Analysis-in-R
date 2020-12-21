library(poLCA)
library(reshape2)
library(plyr)
library(ggplot2)
library(ggparallel)
library(igraph)
library(tidyr)
library(knitr)
mydata<-ProjectData %>% dplyr::select(Head,Spine,Neck,Chest,Abdomen,Shoulder,Elbow,Forearm,Hand,Upperback,Lowerback,Buttock,Thigh,Knee,Calf_Shin,Foot)
mydata<-mydata+1
f<-with(mydata,cbind(Head,Spine,Neck,Chest,Abdomen,Shoulder,Elbow,Forearm,Hand,Upperback,Lowerback,Buttock,Thigh,Knee,Calf_Shin,Foot)~1)
max_II<- -100000
min_bic<- 100000
for (i in 2:10) {
  lc<-poLCA(f,mydata,nclass=i,maxiter=3000,tol=1e-5,na.rm=FALSE,nrep=10,verbose=TRUE,calc.se=TRUE)
  if (lc$bic<-min_bic){
    min_bic<-lc$bic
    LCA_best_model<-lc
  }
}

lc1<-poLCA(f,data=mydata,nclass=1,na.rm=FALSE,nrep=10,maxiter=3000)
lc2<-poLCA(f,data=mydata,nclass=2,na.rm=FALSE,nrep=10,maxiter=3000)
lc3<-poLCA(f,data=mydata,nclass=3,na.rm=FALSE,nrep=10,maxiter=3000)
lc4<-poLCA(f,data=mydata,nclass=4,na.rm=FALSE,nrep=10,maxiter=3000)
lc5<-poLCA(f,data=mydata,nclass=5,na.rm=FALSE,nrep=10,maxiter=3000)
lc6<-poLCA(f,data=mydata,nclass=6,na.rm=FALSE,nrep=10,maxiter=3000)
lc7<-poLCA(f,data=mydata,nclass=7,na.rm=FALSE,nrep=10,maxiter=3000)
lc8<-poLCA(f,data=mydata,nclass=8,na.rm=FALSE,nrep=10,maxiter=3000)

results<-data.frame(Model=c("Model 1"),log_likelihood=lc1$llik,df=lc1$resid.df,BIC=lc1$bic,ABIC=(-2*lc1$llik)+((log((lc1$N +2)/24))*lc1$npar),CAIC=(-2*lc1$llik)+lc1$npar*(1+log(lc1$N)),likelihood_ratio=lc1$Gsq)
results$Model<-as.integer(results$Model)

####Only eight class models will be considered to ensure there are no clusters (classes) which contain only one pain site. (Any cluster solution with more than nine clusters must have at least one cluster with one pain site).

results[1,1]<-c("Model 1")
results[2,1]<-c("Model 2")
results[3,1]<-c("Model 3")
results[4,1]<-c("Model 4")
results[5,1]<-c("Model 5")
results[6,1]<-c("Model 6")
results[7,1]<-c("Model 7")
results[8,1]<-c("Model 8")

results[2,2]<-lc2$llik
results[3,2]<-lc3$llik
results[4,2]<-lc4$llik
results[5,2]<-lc5$llik
results[6,2]<-lc6$llik
results[7,2]<-lc7$llik
results[8,2]<-lc8$llik

results[2,3]<-lc2$resid.df
results[3,3]<-lc3$resid.df
results[4,3]<-lc4$resid.df
results[5,3]<-lc5$resid.df
results[6,3]<-lc6$resid.df
results[7,3]<-lc7$resid.df
results[8,3]<-lc8$resid.df

results[2,4]<-lc2$bic
results[3,4]<-lc3$bic
results[4,4]<-lc4$bic
results[5,4]<-lc5$bic
results[6,4]<-lc6$bic
results[7,4]<-lc7$bic
results[8,4]<-lc8$bic

results[2,5]<- (-2*lc2$llik)+((log((lc2$N+2)/24))*lc2$npar)
results[3,5]<- (-2*lc3$llik)+((log((lc3$N+2)/24))*lc3$npar)
results[4,5]<- (-2*lc4$llik)+((log((lc4$N+2)/24))*lc4$npar)
results[5,5]<- (-2*lc5$llik)+((log((lc5$N+2)/24))*lc5$npar)
results[6,5]<- (-2*lc6$llik)+((log((lc6$N+2)/24))*lc6$npar)
results[7,5]<- (-2*lc7$llik)+((log((lc7$N+2)/24))*lc7$npar)
results[8,5]<- (-2*lc8$llik)+((log((lc8$N+2)/24))*lc8$npar)

results[2,6]<- (-2*lc2$llik)+lc2$npar * (1+log(lc2$N))
results[3,6]<- (-2*lc3$llik)+lc3$npar * (1+log(lc3$N))
results[4,6]<- (-2*lc4$llik)+lc4$npar * (1+log(lc4$N))
results[5,6]<- (-2*lc5$llik)+lc5$npar * (1+log(lc5$N))
results[6,6]<- (-2*lc6$llik)+lc6$npar * (1+log(lc6$N))
results[7,6]<- (-2*lc7$llik)+lc7$npar * (1+log(lc7$N))
results[8,6]<- (-2*lc8$llik)+lc8$npar * (1+log(lc8$N))

results[2,7]<-lc2$Gsq
results[3,7]<-lc3$Gsq
results[4,7]<-lc4$Gsq
results[5,7]<-lc5$Gsq
results[6,7]<-lc6$Gsq
results[7,7]<-lc7$Gsq
results[8,7]<-lc8$Gsq

entropy<-function(p)sum(-p*log(p))
results[1,8]<-c("-")

error_prior<-entropy(lc2$P)
error_post<-mean(apply(lc2$posterior,1,entropy),na.rm=TRUE)
results[2,8]<-round(((error_prior-error_post)/error_prior),3)

error_prior<-entropy(lc3$P)
error_post<-mean(apply(lc3$posterior,1,entropy),na.rm=TRUE)
results[3,8]<-round(((error_prior-error_post)/error_prior),3)

error_prior<-entropy(lc4$P)
error_post<-mean(apply(lc4$posterior,1,entropy),na.rm=TRUE)
results[4,8]<-round(((error_prior-error_post)/error_prior),3)

error_prior<-entropy(lc5$P)
error_post<-mean(apply(lc5$posterior,1,entropy),na.rm=TRUE)
results[5,8]<-round(((error_prior-error_post)/error_prior),3)

error_prior<-entropy(lc6$P)
error_post<-mean(apply(lc6$posterior,1,entropy),na.rm=TRUE)
results[6,8]<-round(((error_prior-error_post)/error_prior),3)

error_prior<-entropy(lc7$P)
error_post<-mean(apply(lc7$posterior,1,entropy),na.rm=TRUE)
results[7,8]<-round(((error_prior-error_post)/error_prior),3)

error_prior<-entropy(lc8$P)
error_post<-mean(apply(lc8$posterior,1,entropy),na.rm=TRUE)
results[8,8]<-round(((error_prior-error_post)/error_prior),3)

colnames(results)<-c("Model","Log-Likelihood","Resid.df","BIC","aBIC","cAIC","Likelihood-Ratio","Entropy")
lca_results<-results
library(ztable)

ztable::ztable(lca_results)

library(forcats)
results$Model<- as_factor(results$Model)
results2<-tidyr::gather(results,Kriterium,Guete,4:7)
results2
fit.plot<-ggplot(results2)+geom_point(aes(x=Model,y=Guete),size=3)+geom_line(aes(Model,Guete,group=1))+theme_bw()+labs(x="",y="",title="Latent Class Analysis Output")+facet_grid(Kriterium~.,scales="free")+theme_bw(base_size=16,base_family="")+theme(panel.grid.major.x=element_blank(),panel.grid.major.y=element_line(colour="grey",size=0.5),legend.title=element_text(size=16,face='bold'),axis.text=element_text(size=16),axis.title=element_text(size=16),legend.text=element_text(size=16),axis.line=element_line(colour="black"))

###Selecting the six-class solution as the best solution

lcmodel <- reshape2::melt(lc6$probs, level=2)

zp1 <- ggplot(lcmodel,aes(x = L2, y = value, fill = Var2))
zp1 <- zp1 + geom_bar(stat = "identity", position = "stack")
zp1 <- zp1 + facet_grid(Var1 ~ .) 
zp1 <- zp1 + scale_fill_brewer(type="seq", palette="Greys") +theme_bw()
zp1 <- zp1 + theme( axis.text.y=element_blank(),
                    +                     axis.ticks.y=element_blank(),                    
                    +                     panel.grid.major.y=element_blank())
Print(zp1)

###Percentages of sample sizes in each cluster for each cluster solution

mp1<-round(colMeans(lc1$posterior)*100)
mp2<-round(colMeans(lc2$posterior)*100)
mp3<-round(colMeans(lc3$posterior)*100)
mp4<-round(colMeans(lc4$posterior)*100)
mp5<-round(colMeans(lc5$posterior)*100)
mp6<-round(colMeans(lc6$posterior)*100)
mp7<-round(colMeans(lc7$posterior)*100)
mp8<-round(colMeans(lc8$posterior)*100)

table(lc6$predclass)

library(nlsem)
library(mclust)

write.csv(lca_results,"data.csv")
write.table(lca_results,"data.scv")

lc6$predclass
ProjectDataPredClass<-cbind(ProjectData,"Predicted LC"=lc6$predclass)
write.csv(ProjectDataPredClass,"ProjectDataPredClass.csv")                          

