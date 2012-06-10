library(irr)

setwd("C:/Users/user/Jrvissoci/Duke")

data<-read.csv("data.csv",header=T)
nhanes<-read.csv("nhanes.csv",header=T)
summary(data)

q1<-data.frame(data$Q1,data$Q1a)
q2<-data.frame(data$Q2,data$Q2a)
q3<-data.frame(data$Q3,data$Q3a)
q4<-data.frame(data$Q4,data$q4a)
q5<-data.frame(data$Q5,data$q5a)
q6<-data.frame(data$Q6,data$q6a)
q7<-data.frame(data$Q7,data$q7a)
q8<-data.frame(data$Q8,data$q8a)
q9<-data.frame(data$Q9,data$q9a)
q10<-data.frame(data$Q10,data$q10a)
q11<-data.frame(data$Q11,data$q11a)
q12<-data.frame(data$Q12,data$q12a)
q13<-data.frame(data$Q13,data$q13a)

kappa2(q1[,1:2], "squared") # predefined set of squared weights
kappa2(q2[,1:2], "squared") # predefined set of squared weights
kappa2(q3[,1:2], "squared") # predefined set of squared weights
kappa2(q4[,1:2], "squared") # predefined set of squared weights
kappa2(q5[,1:2], "squared") # predefined set of squared weights
kappa2(q6[,1:2], "squared") # predefined set of squared weights
kappa2(q7[,1:2], "squared") # predefined set of squared weights
kappa2(q8[,1:2], "squared") # predefined set of squared weights
kappa2(q9[,1:2], "squared") # predefined set of squared weights
kappa2(q10[,1:2], "squared") # predefined set of squared weights
kappa2(q11[,1:2], "squared") # predefined set of squared weights
kappa2(q12[,1:2], "squared") # predefined set of squared weights
kappa2(q13[,1:2], "squared") # predefined set of squared weights


