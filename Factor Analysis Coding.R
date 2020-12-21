attach(ProjectData)
###Changing the NA values to 0 for the principal component analysis
ProjectData[is.na(ProjectData)]<-0
###Principal Component Analysis in R
ProjectData.pca<-princomp(ProjectData)
###Combining the 16 components used in the NorStOP data for a factor analysis
X<-cbind(Head,Spine,Neck,Chest,Abdomen,Shoulder,Elbow,Forearm,Hand,Upperback,Lowerback,Buttock,Thigh,Knee,Calf_Shin,Foot)
###Principle Component Analysis of the 16 variables
X.pca<-princomp(X)
###Summarizing the Principle Component Analysis of the 16 variables
summary(X.pca)
###Plotting the Principle Component Analysis of the 16 Variables
plot(X.pca)
###ScreePlot of the Factors
screeplot(X.pca,type="line",main="Scree Plot")
###Factor Analysis of the 16 variables, using Varimax Rotation with 4 factors
X.fa<-factanal(X,factors=4,rotation="varimax")
###Comparing the factors to the components
X.fa
X.far<-factanal(X,factors=4,rotation="varimax",scores="regression")
###Using the Polycor Function
het.mat<-hetcor(X)$cor
het.mat
fa.1<-factanal(covmat=het.mat,factors=4,rotation="varimax")
fa.1
fa.2<-fa(r=het.mat,nfactors=4,n.obs=nrow(X),rotate="varimax")
fa.2
plot(fa.2)
fa.diagram(fa.2)
pc<-hetcor(X,ML=TRUE)
library(psych)
faPC<-fa(r=pc$correlations,nfactors=4,n.obs=12408,rotate="varimax")
faPC$loadings

                         
