# load package
require(RCurl)
# read script lines from website
SEM1 <- "https://raw.github.com/joaovissoci/Data/master/ScriptDatabaseSEM.R"
script <- getURL(SEM1, ssl.verifypeer = FALSE)
eval(parse(text = script))
# clean-up
rm("script", "SEM1")
# check
ls()
#[1] "NAME OF THE DATABASE"
#Type the name of the database checked

mean (SEMdatabase)
sd(SEMdatabase)
var(SEMdatabase)

#Teste

sum(!is.na(SEMdatabase))
SEMdatabase<-edit(SEMdatabase)
summary(SEMdatabase)


boxplot(SEMdatabase$MERE,SEMdatabase$MEIn,SEMdatabase$MEId,SEMdatabase$MIAO,SEMdatabase$MIEE,SEMdatabase$MIC)
title(sub="1-External Regulation,2-Introjection,3-Identification,4-Reach Goals,5-Stimulating Experiences,6-Knowledge",xlab="Self-determined Motivation")
boxplot(SEMdatabase$RP,SEMdatabase$SP,SEMdatabase$OP,SEMdatabase$RM,SEMdatabase$SM,SEMdatabase$OM)
title(xlab="Perceived Parental Support")
boxplot(SEMdatabase$SCI)
title(xlab="Sport Confrontation Index")
