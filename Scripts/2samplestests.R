daily.intake <- c(5260,5470,5640,6180,6390,6515,6805,7515,7515,8230,8770)
daily.intake
wilcox.test(daily.intake, mu=7725)

require(ISwR)
attach(energy)
energy
var.test(expend~stature)
t.test(expend~stature) #Variances not equivalent
t.test(expend~stature, var.equal=T)
wilcox.test(expend~stature)#nonparametric distribution
attach(intake)
intake
t.test(pre,post, paired=T)
summary(intake)
wilcox.test(pre,post,paired=T)