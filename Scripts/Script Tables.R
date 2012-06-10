
caff.marital<-matrix(c(652,1537,598,242,36,46,38,21,218,327,106,67),nrow=3,byrow=T)

caff.marital

colnames(caff.marital)<-c("0","1-150","151-300",">300")
rownames(caff.marital)<-c("Married","Prev.Married","Single")
caff.marital

names(dimnames(caff.marital))<-c("marital","consumption")
caff.marital

as.table(caff.marital)

as.data.frame(as.table(caff.marital))

library(ISwR)
attach(juul)

table(sex)
table(sex,menarche)
table(menarche,tanner)
table(sex,menarche,tanner)

detach(juul)

xtabs(~tanner+sex,data=juul)
xtabs(~dgn+diab+coma,data=stroke)

ftable(coma+diab~dgn,data=stroke)

attach(juul)
tanner.sex<-table(tanner,sex)
tanner.sex
margin.table(tanner.sex,1)
margin.table(tanner.sex,2)

prop.table(tanner.sex,1)

prop.table(tanner.sex,1)*100

tanner.sex/sum(tanner.sex,1)*100

total.caff<-margin.table(caff.marital,2)
total.caff

barplot(total.caff)
barplot(total.caff,col="white", beside=T)

par(mfrow=c(2,2))
barplot(caff.marital)
barplot(t(caff.marital))
barplot(t(caff.marital),beside=T)
barplot(prop.table(t(caff.marital),2),beside=T)
par(mfrow=c(1,1))

barplot(prop.table(t(caff.marital),2),beside=T,legend.text=colnames(caff.marital),col=c("white","grey80","gray50","black"))

locator()

dotchart(t(caff.marital),lcolocr="black")

opar<-par(mfrow=c(2,2),mex=0.8,mar=c(1,1,2,1))
slices<-c("white","gray80","gray50","black")
pie(caff.marital["Married",],main="Married",col=slices)
pie(caff.marital["Prev.Married",],main="Previously Married",col=slices)
pie(caff.marital["Single",],main="Single",col=slices)
par(opar)