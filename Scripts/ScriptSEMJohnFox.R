Script R - SEM - Modelo John Fox

# MODEL II

# Install the package 'sem'
# carry data library

library (sem)   

# Change R's directory to that in which the data base .CSV file is 
# Load database

SEM<- read.csv("BaseSEMSuporte.csv",header=T)
SEM

# sem package model specification
mod.wh.1 <- specify.model()# Type these values that specify the model's relations (just use de Ctrl+R over each relation).
Factor1->Q1,NA,1
Factor1->Q2,fa1,NA
Factor1->Q3,fa2,NA
Factor1->Q4,fa3,NA
Factor1->Q5,fa4,NA
Factor1->Q6,fa5,NA
Factor1<->Factor1,er1,NA
Q1<->Q1,oer1,NA
Q2<->Q2,oer2,NA
Q3<->Q3,oer3,NA
Q4<->Q4,oer4,NA
Q5<->Q5,oer5,NA
Q6<->Q6,oer6,NA

# Insert de covariance matrix

cov.teste <- cov(msts, y = NULL, use = "everything", method = c("pearson", "kendall", "spearman"))
cov.teste
msts


# Estimate de model (Here is where we have been finding difficulties)

sem.wh.1 <- sem(mod.wh.1, cov.teste, N=67)
summary(sem.wh.1)