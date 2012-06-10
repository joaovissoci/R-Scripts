setwd("C:/Users/user/Google Drive/Projeto Duke/Artigo SBC")
data<-read.csv("SBCData.csv",header=T)
data1<-read.csv("SBCData1.csv",header=T)
head(data)
attach(data)
detach(data)
data2<-read.csv("SBCData2.csv",header=T)
qd<-read.csv("SBCDataqd.csv",header=T)

#Bar Plots together

library(lattice)
histogram(~mns | ssz, data = data.frame(mns = c(mns),
                                           + ssz = gl(3, 1000, labels = c("1", "4", "16"))),
            + layout = c(3, 1), main = "Histograms of means by sample size")


#Other options

data(Chem97, package = "mlmRev")
head(Chem97)
histogram(~ gcsescore, data = Chem97)
histogram(~ gcsescore | factor(gender), data = Chem97)

VADeathsDF <- as.data.frame.table(VADeaths, responseName = "Rate")
VADeathsDF
barchart(Var1 ~ Rate | Var2, VADeathsDF, layout = c(4, 1))
barchart(Var1 ~ Rate | Var2, VADeathsDF, layout = c(4, 1), origin = 0)

barchart(Centros ~ Score | Type, data, origin = 0)
dotplot(Centros ~ Score | Type, data, origin = 0)

dotplot(Var1 ~ Rate | Var2, VADeathsDF, layout = c(4, 1))
dotplot(Var1 ~ Rate, data = VADeathsDF, groups = Var2, type = "o",
        auto.key = list(space = "right", points = TRUE, lines = TRUE))

#Polar Histogram

# Star Plot
require(plotrix)

testlen <- c(sin(seq(0,1.98*pi,length=100))+2+rnorm(100)/10)
testpos <- seq(0,1.98*pi,length=100)
testlen
testpos

radial.plot (qd$Score,qd$Centers,rp.type="p",main="Test Polygon",line.col="blue")

# Density Plot
require(vcd)

data("Arthritis")
Arthritis

spine(Centros ~ Score, 
      data = data, 
      breaks = c(0,5,10,15,20,60,110))

palette(rainbow(12, s = 0.6, v = 0.75))
stars(star[, 1:5], len = 0.8,
      main = "Coaching Production", draw.segments = TRUE,scale=FALSE)

palette(rainbow(12, s = 0.6, v = 0.75))
stars(mtcars[1, ], len = 0.8,
      main = "Motor Trend Cars", draw.segments = TRUE)

star<-rbind(c(106,49,20,4,5))
star<-data.frame(star)
star

palette(rainbow(12, s = 0.6, v = 0.75))
stars(mtcars[, 1:7], len = 0.8, key.loc = c(12, 1.5),
      main = "Motor Trend Cars", draw.segments = TRUE)
stars(mtcars[, 1:7], len = 0.6, key.loc = c(1.5, 0),
      main = "Motor Trend Cars", draw.segments = TRUE,
      frame.plot=TRUE, nrow = 4, cex = .7)

stars(star[,1:5], locations = 0:1, scale=FALSE,
      draw.segments = TRUE, col.segments=0, col.stars=1:10,key.loc= 0:1,
      main="US Judges 1-10 ")


#Graphs Try out

#With total
barchart(Centros ~ Score | Type, data, origin = 0)
dotplot(Centros ~ Score | Type, data, origin = 0)

barchart(Type ~ Score | Centros, data, origin = 0)
dotplot(Type ~ Score | Centros, data, origin = 0)


#Without TOtal
barchart(Centros ~ Score | Type, data2, origin = 0)
dotplot(Centros ~ Score | Type, data2, origin = 0)

#Density Plot

spine(Improved ~ Age, 
      data = Arthritis, 
      breaks = quantile(Arthritis$Age))

#Polar Hist

library(plyr)
library(ggplot2)
source("polarHistogram.R")
data3<-read.csv("polarhist.csv",header=T)
df
data
family<-data3$Family
item<-data3$Item
score<-data3$Score
value<-data3$Value

df<-data.frame(family,item,score,value)

p<-polarHistogram(df,familyLabel=FALSE)
print(p)

## scale linearly (not affinely) to [0, 1]
USJudge <- apply(USJudgeRatings, 2, function(x) x/max(x))
Jnam <- row.names(USJudgeRatings)
Snam <- abbreviate(substring(Jnam,1,regexpr("[,.]",Jnam) - 1), 7)
stars(USJudge, labels = Jnam, scale = FALSE,
      key.loc = c(13, 1.5), main = "Judge not ...", len = 0.8)
stars(USJudge, labels = Snam, scale = FALSE,
      key.loc = c(13, 1.5), radius = FALSE)

loc <- stars(USJudge, labels = NULL, scale = FALSE,
             radius = FALSE, frame.plot = TRUE,
             key.loc = c(13, 1.5), main = "Judge not ...", len = 1.2)
text(loc, Snam, col = "blue", cex = 0.8, xpd = TRUE)

## 'Segments':
stars(USJudge, draw.segments = TRUE, scale = FALSE, key.loc = c(13,1.5))

## 'Spider':
stars(USJudgeRatings, locations=c(0,0), scale=FALSE,radius = FALSE,
      col.stars=1:10, key.loc = c(0,0), main="US Judges rated")
## Same as above, but with colored lines instead of filled polygons.
stars(USJudgeRatings, locations=c(0,0), scale=FALSE,radius = FALSE,
      col.lines=1:10, key.loc = c(0,0), main="US Judges rated")
## 'Radar-Segments'
stars(USJudgeRatings[1:10,], locations = 0:1, scale=FALSE,
      draw.segments = TRUE, col.segments=0, col.stars=1:10,key.loc= 0:1,
      main="US Judges 1-10 ")
palette("default")
stars(cbind(1:16,10*(16:1)),draw.segments=TRUE,
      main = "A Joke -- do *not* use symbols on 2D data!")

testlen<-star
testpos<-c("Qds", "Redigido", "publicado", "Projto", "Financ")
testlab<-letters[1:10]
oldpar<-radial.plot(testlen,testpos,main="Test Radial Lines",line.col="red",lwd=3)
testlen<-c(sin(seq(0,1.98*pi,length=100))+2+rnorm(100)/10)
testpos<-seq(0,1.98*pi,length=100)
radial.plot(testlen,testpos,rp.type="p",main="Test Polygon",line.col="blue")

testlen<-rnorm(10)*2+5
testpos<-seq(0,18*pi/10,length=10)
testlab<-letters[1:10]
oldpar<-radial.plot(testlen,testpos,main="Test Radial Lines",line.col="red",lwd=3)
>  testlen<-c(sin(seq(0,1.98*pi,length=100))+2+rnorm(100)/10)
>  testpos<-seq(0,1.98*pi,length=100)
>  radial.plot(testlen,testpos,rp.type="p",main="Test Polygon",line.col="blue")

posmat<-matrix(sample(2:9,30,TRUE),nrow=3)
radial.plot(star,labels=testpos,rp.type="p",
               main="Spiderweb plot",line.col=2:4,show.grid=FALSE,lwd=1:3,
               radial.lim=c(0,10))
star<-as.matrix(star)
