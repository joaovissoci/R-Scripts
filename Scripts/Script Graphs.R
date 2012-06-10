SEM1<- read.csv("BaseSEM.csv",header=T)
#intall gridExtra,wq packages

library(ggplot2)
library(grid)
library(gridExtra)
library(wq)


a<-qplot(MERE,MEIn, data = SEM1)
(a1<-a+geom_smooth(method="loess",size=1.5))
b<-qplot(MERE,MEId, data = SEM1)
(b1<-b+geom_smooth(method="loess",size=1.5))
c<-qplot(MERE,MIAO, data = SEM1)
(c1<-c+geom_smooth(method="loess",size=1.5))
d<-qplot(MERE,MIEE, data = SEM1)
(d1<-d+geom_smooth(method="loess",size=1.5))
e<-qplot(MERE,MIC, data = SEM1)
(e1<-e+geom_smooth(method="loess",size=1.5))
f<-qplot(MERE,SCI, data = SEM1)
(f1<-f+geom_smooth(method="loess",size=1.5))
g<-qplot(MERE,RP, data = SEM1)
(g1<-g+geom_smooth(method="loess",size=1.5))
h<-qplot(MERE,SP, data = SEM1)
(h1<-h+geom_smooth(method="loess",size=1.5))
i<-qplot(MERE,OP, data = SEM1)
(i1<-h+geom_smooth(method="loess",size=1.5))
j<-qplot(MERE,RM, data = SEM1)
(j1<-h+geom_smooth(method="loess",size=1.5))
k<-qplot(MERE,SM, data = SEM1)
(k1<-h+geom_smooth(method="loess",size=1.5))
l<-qplot(MERE,OM, data = SEM1)
(l1<-h+geom_smooth(method="loess",size=1.5))

plot5 <- grid.arrange(a, b, heights=c(3/4, 1/4), ncol=1, nrow=2)

layOut(list(a, 1:3, 1),   # takes three rows and the first column
        list(b, 1, 2),    # next three are on separate rows
         list(c, 2,2), 
          list(d, 3,2))