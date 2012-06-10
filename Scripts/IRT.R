install.packages("ltm")
library(ltm)

LSAT
descript(LSAT) #descriptives


fit1 <- rasch(LSAT, constraint = cbind(length(LSAT) + 1, 1)) #fitting rasch model
summary(fit1) #Show the summary for the rasch model fitting

coef(fit1, prob = TRUE, order = TRUE) #Transform de difficulties into probabilites

GoF.rasch(fit1, B = 199) #Check the fit of the model

margins(fit1) #Alternative fit check, using two or threee way X2 residuals
margins(fit1, type = "three-way", nprint = 2) #fitting for 3way ckech 

fit2 <- rasch(LSAT) #Unconstrained method/ 1 slope for the whole data
summary(fit2)

anova(fit1,fit2)#comparing discrimination rates

margins(fit2, type = "three-way", nprint = 2)

fit3 <- ltm(LSAT ~ z1) #a Two=parameter logistic model - LSAT=dichotomus variavle/z1=latent variable
summary (fit3)
anova(fit2, fit3)

fit4 <- tpm(LSAT, type = "rasch", max.guessing = 1) #Three Parameter logistic model with a guessing parameter
summary(fit4)
anova(fit2, fit4)

par(mfrow = c(2, 2))
plot(fit2, legend = TRUE, cx = "bottomright", lwd = 3,
cex.main = 1.5, cex.lab = 1.3, cex = 1.1)
plot(fit2, type = "IIC", annot = FALSE, lwd = 3, cex.main = 1.5,
cex.lab = 1.3)
plot(fit2, type = "IIC", items = 0, lwd = 3, cex.main = 1.5,
cex.lab = 1.3)
plot(0:1, 0:1, type = "n", ann = FALSE, axes = FALSE)
info1 <- information(fit2, c(-4, 0))
info2 <- information(fit2, c(0, 4))
text(0.5, 0.5, labels = paste("Total Information:", round(info1$InfoTotal, 3),
"\n\nInformation in (-4, 0):", round(info1$InfoRange, 3),
paste("(", round(100 * info1$PropRange, 2), "%)", sep = ""),
"\n\nInformation in (0, 4):", round(info2$InfoRange, 3),
paste("(", round(100 * info2$PropRange, 2), "%)", sep = "")), cex = 1.5)

factor.scores(fit2) #ability to estimate 
factor.scores(fit2, resp.patterns = rbind(c(0,1,1,0,0), c(0,1,0,1,0))) #ability for nonspecific responde patterns

#IRT for ordinal variables
rcor.test(Environment, method = "kendall") #non parametric correlation, just ans alternativo to de descriptive function

fit1 <- grm(Environment, constrained = TRUE)
fit1

margins(fit1)
margins(fit1, type = "three")

fit2 <- grm(Environment)
fit2

anova(fit1, fit2)

information(fit2, c(-4, 4)) #check numerically the probabilities

information(fit2, c(-4, 4), items = c(1, 6))

par(mfrow = c(2, 2))
plot(fit2, lwd = 2, cex = 1.2, legend = TRUE, cx = "left",
+ xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)
plot(fit2, type = "IIC", lwd = 2, cex = 1.2, legend = TRUE, cx = "topleft",
+ xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)
plot(fit2, type = "IIC", items = 0, lwd = 2, xlab = "Latent Trait",
+ cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)
info1 <- information(fit2, c(-4, 0))
info2 <- information(fit2, c(0, 4))
text(-1.9, 8, labels = paste("Information in (-4, 0):",
+ paste(round(100 * info1$PropRange, 1), "%", sep = ""),
+ "\n\nInformation in (0, 4):",
+ paste(round(100 * info2$PropRange, 1), "%", sep = "")), cex = 1.2)

fit1 <- ltm(WIRS ~ z1 + z2)
fit2 <- ltm(WIRS ~ z1 * z2) #interaction between the latent variables improved the fit
fit2
anova(fit1, fit2)

par(mfrow = c(2, 2))
plot(fit2, category = 1, lwd = 2, cex = 1.2, legend = TRUE, cx = -4.5,
+ cy = 0.85, xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3,
+ cex.axis = 1.1)
for (ctg in 2:3) {
+ plot(fit2, category = ctg, lwd = 2, cex = 1.2, annot = FALSE,
+ xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3,
+ cex.axis = 1.1)
+ }
