setwd("C:/Users/user/Jrvissoci/Duke/WhoQOL Validation - IRT/WHOQL - Chinese Maley")

library(psych)
ce_chinese1<-read.csv("ce_chineseSQ.csv",header=T)
ce_chinese<-na.omit(ce_chinese1)
ce_chinese
ce_chinese2<-read.csv("ce_chineseFact1.csv",header=T)
ce_chinese3<-na.omit(ce_chinese2)
ce_chinese3

#Determine the number of factors
library(nFactors)

par(mfrow=c(2,2))
ev <- eigen(cor(ce_chinese)) # get eigenvalues
ev
ap <- parallel(subject=nrow(ce_chinese),var=ncol(ce_chinese),rep=100,cent=.05)
nS <- nScree(ev$values)
plotnScree(nS)

#KMO
kmo = function( ce_chinese ){

  library(MASS)
  X <- cor(as.matrix(ce_chinese))
  iX <- ginv(X)
  S2 <- diag(diag((iX^-1)))
  AIS <- S2%*%iX%*%S2                      # anti-image covariance matrix
  IS <- X+AIS-2*S2                         # image covariance matrix
  Dai <- sqrt(diag(diag(AIS)))
  IR <- ginv(Dai)%*%IS%*%ginv(Dai)         # image correlation matrix
  AIR <- ginv(Dai)%*%AIS%*%ginv(Dai)       # anti-image correlation matrix
  a <- apply((AIR - diag(diag(AIR)))^2, 2, sum)
  AA <- sum(a)
  b <- apply((X - diag(nrow(X)))^2, 2, sum)
  BB <- sum(b)
  MSA <- b/(b+a)                        # indiv. measures of sampling adequacy

  AIR <- AIR-diag(nrow(AIR))+diag(MSA)  # Examine the anti-image of the
                                        # correlation matrix. That is the
                                        # negative of the partial correlations,
                                        # partialling out all other variables.

  kmo <- BB/(AA+BB)                     # overall KMO statistic

  # Reporting the conclusion
    if (kmo >= 0.00 && kmo < 0.50){
      test <- 'The KMO test yields a degree of common variance
unacceptable for FA.'
    } else if (kmo >= 0.50 && kmo < 0.60){
      test <- 'The KMO test yields a degree of common variance miserable.'
    } else if (kmo >= 0.60 && kmo < 0.70){
      test <- 'The KMO test yields a degree of common variance mediocre.'
    } else if (kmo >= 0.70 && kmo < 0.80){
      test <- 'The KMO test yields a degree of common variance middling.'
    } else if (kmo >= 0.80 && kmo < 0.90){
      test <- 'The KMO test yields a degree of common variance meritorious.'
    } else {
      test <- 'The KMO test yields a degree of common variance marvelous.'
    }

    ans <- list(  overall = kmo,
                  report = test,
                  individual = MSA,
                  AIS = AIS,
                  AIR = AIR )
    return(ans)

}    # end of kmo()
kmo(ce_chinese)


#FACTOR EXTRACTION

library(GPArotation)

fa(ce_chinese,nfactors=5,rotate="varimax")
fa(ce_chinese,nfactors=4,rotate="varimax")
fa(ce_chinese,nfactors=3,rotate="varimax")
fa(ce_chinese,nfactors=2,rotate="varimax")
fa(ce_chinese,nfactors=1,rotate="varimax")

fa(ce_chinese,nfactors=5,rotate="promax")
fa(ce_chinese,nfactors=4,rotate="promax")
fa(ce_chinese,nfactors=3,rotate="promax")
fa(ce_chinese,nfactors=2,rotate="promax")
fa(ce_chinese,nfactors=1,rotate="promax")

fa(ce_chinese,nfactors=6)
fa(ce_chinese,nfactors=5)
fa(ce_chinese,nfactors=4)
fa(ce_chinese,nfactors=3)
fa(ce_chinese,nfactors=2)
fa(ce_chinese,nfactors=1)

#Communalities

1*(1 - fit5$uniquenesses)
1*(1 - fit3$uniquenesses)
1*(1 - fit4$uniquenesses)

# CFA
library (sem)

mod.wh.2 <- specify.model()# Type these values that specify the model's relations (just use de Ctrl+R over each relation).

#Latent Variables
QV->q3,qvf1,NA
QV->q10,qvf2,NA
QV->q15,qvf4,NA
QV->q16,qvf5,NA
QV->q17,qvf6,NA
QV->q18,qvf7,NA
QV->q5,qvp1,NA
QV->q6,qvp2,NA
QV->q7,qvp3,NA
QV->q11,qvp4,NA
QV->q19,NA,1
QV->q20,qvs1,NA
QV->q21,NA,1
QV->q22,qvs3,NA
QV->q8,qve1,NA
QV->q9,qve2,NA
QV->q12,qve3,NA
QV->q13,qve4,NA
QV->q14,qve5,NA
QV->q23,qve6,NA
QV->q24,qve7,NA
QV->q25,qve8,NA
QV<->QV,e1,NA
q3<->q3,eob1,NA
q5<->q5,eob3,NA
q6<->q6,eob4,NA
q7<->q7,eob5,NA
q8<->q8,eob6,NA
q9<->q9,eob7,NA
q10<->q10,eob8,NA
q11<->q11,eob9,NA
q12<->q12,eob10,NA
q13<->q13,eob11,NA
q14<->q14,eob12,NA
q15<->q15,eob13,NA
q16<->q16,eob14,NA
q17<->q17,eob15,NA
q18<->q18,eob16,NA
q19<->q19,eob17,NA
q20<->q20,eob18,NA
q21<->q21,eob19,NA
q22<->q22,eob20,NA
q23<->q23,eob21,NA
q24<->q24,eob22,NA
q25<->q25,eob23,NA
q5<->q6,cov1,NA
q18<->q17,cov2,NA
q9<->q8,cov3,NA
q13<->q12,cov4,NA
q24<->q23,cov5,NA
q24<->q25,cov6,NA
q21<->q20,cov7,NA
q8<->q7,cov8,NA
q17<->q16,cov9,NA
q20<->q19,cov10,NA
q15<->q3,cov11,NA
q10<->q7,cov12,NA
q22<->q20,cov13,NA
q22<->q21,cov14,NA
q12<->q11,cov15,NA

# Insert de covariance matrix
cov.chinese1 <- cov(ce_chinese3, y = NULL, use = "everything", method = c("pearson", "kendall", "spearman"))


sem.wh.2 <- sem(mod.wh.2, cov.chinese1, N=416)

summary(sem.wh.2)

mod.indices(sem.wh.2)

