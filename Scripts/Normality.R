setwd("C:/Users/user/Jrvissoci/Duke")

data<-read.csv("data.csv",header=T)
summary(data)
dim(data)

library(moments)                  # load the moments package 
kurtosis(data$Q1) - 3            # apply the kurtosis function
kurtosis(data$Q2) - 3
kurtosis(data$Q3) - 3
kurtosis(data$Q4) - 3
kurtosis(data$Q5) - 3
kurtosis(data$Q6) - 3
kurtosis(data$Q7) - 3
kurtosis(data$Q8) - 3
kurtosis(data$Q9) - 3
kurtosis(data$Q10) - 3
kurtosis(data$Q11) - 3
kurtosis(data$Q12) - 3
kurtosis(data$Q13) - 3

skewness(data$Q1)
skewness(data$Q2)
skewness(data$Q3)
skewness(data$Q4)
skewness(data$Q5)
skewness(data$Q6)
skewness(data$Q7)
skewness(data$Q8)
skewness(data$Q9)
skewness(data$Q10)
skewness(data$Q11)
skewness(data$Q12)
skewness(data$Q13)

library(nortest)

ad.test(data$Q1)
ad.test(data$Q2)
ad.test(data$Q3)
ad.test(data$Q4)
ad.test(data$Q5)
ad.test(data$Q6)
ad.test(data$Q7)
ad.test(data$Q8)
ad.test(data$Q9)
ad.test(data$Q10)
ad.test(data$Q11)
ad.test(data$Q12)
ad.test(data$Q13)

