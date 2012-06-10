m <- cbind(1, 1:7) # the '1' (= shorter vector) is recycled
m

data<-read.csv("ely.csv",header=T)
data1<-read.csv("ely1.csv",header=T)
data1

cor.test(~ data$APre+data$Apos+data$ATPre+data$ATpos+data$IFPre+data$IFPos,data=data)
summary(data)

panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
  
  test <- cor.test(x,y)
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " "))
  
  text(0.5, 0.5, txt, cex = cex * r)
  text(.8, .8, Signif, cex=cex, col=2)
}
pairs(data[,c(1,2,3,4,5,6)], 
      lower.panel=panel.smooth, upper.panel=panel.cor)


require(hdrcde)
require(vioplot)
require(Hmisc)

opar <- par(mfrow=c(1,5), mar=c(3,2,4,1))

plot(yyy, xxx, type="l",main="Underlying\ndensity")

boxplot(data, col="gray90")


par(opar)


