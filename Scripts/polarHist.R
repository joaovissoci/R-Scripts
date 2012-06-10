setwd("C:/Users/user/Jrvissoci/Duke/Burn Scales - Systematic Review/Data")
library(plyr)
library(ggplot2)
source("polarHistogram.R")
data<-read.csv("burn3.csv",header=T)
df
data
family<-data$Family
item<-data$Item
score<-data$Score
value<-data$Value

df<-data.frame(family,item,score,value)

p<-polarHistogram(df,familyLabel=FALSE)
print(p)