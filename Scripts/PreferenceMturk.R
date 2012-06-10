#######################################################################################
#Analysis script for the Preference Manuscript
#######################################################################################

#setting environment -------------------------------------------------------------------
#remove all objects and then check
rm(list = ls())
ls()
#dettach all packages
detach()

#Instal packages needes for the analysis
lapply(c("ggplot2", "psych", "RCurl", "irr", "nortest", "moments"), library, character.only=T)

#uploading data ------------------------------------------------------------------------
#Functions to pull the dara from the internet file 
options(RCurlOptions = list(capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"), ssl.verifypeer = FALSE))
#see http://goo.gl/mQwxO on how to get this link
#link below won't work until data is entered in the right format
preference.tto <- getURL("https://docs.google.com/spreadsheet/pub?hl=en&hl=en&key=0AoTReYGK49h_dHlYQkNXaExEa293eEE5MmxzU2tfV3c&output=csv")
read.csv(textConnection(preference.tto))
names(preference.tto)

preference.slider <- getURL("https://docs.google.com/spreadsheet/pub?key=0AoTReYGK49h_dFpMV1RjNXI0bnhRSzBxaVJITFJ5d3c&output=csv")
read.csv(textConnection(preference.slider))
names(preference.slider)

#checking data set as a whole
attach(preference.tto)
str (preference.tto)
head(preference.tto)

#Exploratory data analyses --
summary(     )#This comand will provide a whole set of descriptive results for each variables
mean<-tapply(y, A, mean) #Get descriptive (mean for example) values by factors; y = numeric data; A=factors;
freq() #This comand will provide the relative distribution of the data
ad.test() # Anderson-Darling test for normality
skewness() #Will provide skweness analysis
kurtosis() - 3 #Will provide kurtosis analysis
qplot() # histogram plot
pwr.anova.test(k = , n = , f = , sig.level = , power = )#Power analysis for variance analysis
boxplot() #will provide a boxplot for the variables to analysis potential outliers 

#Variance analysis for a normal distribution
anova <- aov(y ~ A, data=) # y = numeric data; A=factors; ~ stands for "defined by, so the formula says y(numeric variable) defined by A (factor variable)
summary(anova)
#Tukey's Post hoc
TukeyHSD(anova)
#Nonparametric variance analysis
kruskal.test(y ~ A, data=) # y = numeric data; A=factors; ~ stands for "defined by, so the formula says y(numeric variable) defined by A (factor variable

#T-test for pairwiase comparison
t.test(y~A)

#Nonparametric paierwise comparison
wilcox.test(y, A, conf.int = TRUE) # y = numeric data; A=factors

#######################################################################################
#ERAS.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. You are free: to Share — to copy, distribute and transmit the work to Remix — to adapt the work, under the following conditions: Attribution — You must attribute the work in the manner specified by the author or licensor (but not in any way that suggests that they endorse you or your use of the work). Noncommercial — You may not use this work for commercial purposes. With the understanding that: Waiver — Any of the above conditions can be waived if you get permission from the copyright holder. Public Domain — Where the work or any of its elements is in the public domain under applicable law, that status is in no way affected by the license. Other Rights — In no way are any of the following rights affected by the license: Your fair dealing or fair use rights, or other applicable copyright exceptions and limitations; The author's moral rights; Rights other persons may have either in the work itself or in how the work is used, such as publicity or privacy rights. Notice — For any reuse or distribution, you must make clear to others the license terms of this work. The best way to do this is with a link to this web page. For more details see http://creativecommons.org/licenses/by-nc/3.0/
#######################################################################################
