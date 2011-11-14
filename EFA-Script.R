
#Install Hmisc Package
library(Hmisc)
SEM1<- read.csv("BaseSEM.csv",header=T)
SEM1
fit <- factanal(SEM1, 5, rotation="varimax",scores="regression")
print(fit, digits=2, cutoff=.3, sort=TRUE)
colnames(fit$loadings)<-c("#Nome do Fator","#Nome do Fator2","#Nome do Fator3")
print(fit, digits=2, cutoff=.3, sort=TRUE)

#Determine the number of factors
library(nFactors)
ev <- eigen(cor(SEM1)) # get eigenvalues
ap <- parallel(subject=nrow(SEM1),var=ncol(SEM1),rep=100,cent=.05)
nS <- nScree(ev$values, ap$eigen$qevpea)
plotnScree(nS)
