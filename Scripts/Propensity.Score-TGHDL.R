#Install 
library(nonrandom)

PSData<-read.csv("PSData.csv",header=T)
PSData

#SELECTING THE PS MODEL - relative effect

effect <- relative.effect(data = PSData,sel = c(2:14),family = "binomial",resp = 15,treat = "PCR_RSV")

PSData.effect <- relative.effect(data = PSData,
    formula = obito_r~tghdldic+Sexo_r+Idade+causa_r+Exp_razao+Follow+
      Dm_r+Tabag_r+Colest_r+Trig_r+HAS_r+Sed_r+Alc_r+
      Estresse+peso+alt+IMC+DAC+PAS+PAD+Cint+Tempo+Vo2+c_t+ldl
      +hdl+vldl+glic+trigli+tg_hdl+Escore_fram+Fram+Class_risk+SM) #pst=outcome/therapie=grouping/tgr and age=covariates
PSData.effect

#PROPENSITY SCORE ESTIMATION

vx = vx[!is.na(PSData)]
PSData.ps <- pscore(data = PSData,formula = tghdldic~Sexo_r+Idade,name.pscore="ps")
PSData.ps$pscore


pscore.plot(object=PSData.ps,
    main="Density Plot",
    with.legend=TRUE,
    cex.main=1.6, cex.axis=1.4, legend.cex=1.25, cex.lab=1.5,
    par.1=list(lty=1, lwd=2), par.0=list(lty=3, lwd=2),
    xlab="",
    ylim=c(0,4.5))


#STRATIFICATION BY PSCORE
PSData.str<- ps.makestrata(object = PSData.ps,
seq(0,1,0.2),na.rm=T,
name.stratum.index = "stratum")
PSData.str

#MATCHING BY PS
PSData.m <- ps.match(object = PSData.ps,
    ratio = 1, x = 0.2, caliper = "logit",
    matched.by = "ps", setseed = 38902)
PSData.m

# BALANCE CHECK

## Figure 2 (left)
plot1 <- dist.plot(object = PSData.m,   
                   plot.type = 1, compare = TRUE,
  bar.cex = 1.2, legend.cex=1.5, sub.cex=1.2,
  label.match = c("original data","matched sample"),
  col=c("gray65", "gray35"))
>
> ## Figure 2 (right)
plot2 <- dist.plot(object = PSData.m,  plot.type = 2, compare = TRUE,
 bar.cex = 1.2, legend.cex=1.2, sub.cex=1.2,
 legend.title = "Therapy",
 col=c("gray65", "gray35"))

# TREATMENT EFFECTS - PS.Estimate

estimate <- ps.estimate(object = PSData.m,
resp = "obito_r",
family = "binomial")
estimate

#Important Arguments
# $data/$data.matched


