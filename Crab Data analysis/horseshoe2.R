horseshoe=read.csv("C:\\Users\\manub\\Downloads\\CDA\\crab.csv")
prob=as.numeric(horseshoe$Sa>0)
horseshoe1=data.frame(horseshoe,prob)
horse.glm=glm(formula = horseshoe1$prob ~horseshoe1$Wt , family = binomial(link = "logit"))
library("aod")
wald=wald.test(b = coef(horse.glm), Sigma = vcov(horse.glm),Terms = 2:2)
#Grouping the Wts
library(ggplot2)
library(GGally)
library(MASS)
library(survey)
library(tidyr)
horse.cat <-  transform(horseshoe1, group=cut(Wt,breaks=9))

horse.agregeate <- do.call(data.frame,aggregate(cbind(Wt,prob)~group, horse.cat, 
                                         FUN=function(x) c(Count=length(x), Sum=sum(x), Mean = mean(x))))


#fitting the model to the grouped variables

horse.cat.glm = glm(prob.Mean~Wt.Mean,weights = Wt.Count, family = binomial(link = "logit"),data = horse.agregeate)
summary(horse.cat.glm)


results = data.frame(Wt = horse.agregeate$Wt.Mean, horse.agregeate$Wt.Count, observed = horse.agregeate$prob.Mean, horse.agregeate$prob.Sum, fitted = fitted(horse.cat.glm), residuals(horse.cat.glm),predict(horse.cat.glm))

results.req = data.frame(Wt = horse.agregeate$Wt.Mean, observed = horse.agregeate$prob.Mean, fitted = fitted(horse.cat.glm))
results.long = results.req %>% gather(type, Sa_Mean, fitted:observed )

fit_line = ggplot(data=results.req, aes(x = Wt, y = fitted)) + geom_line(color = "Blue",)

fit_line + geom_point(data=results.req, aes(x = Wt, y = observed, color = "Red"))

ggplot(data = results.long, aes(x = Wt, y =Sa_Mean, color = type)) + geom_line() + geom_point()
residualPlots(horse.cat.glm)
influenceIndexPlot(horse.cat.glm,id.n = 2)
influencePlot(horse.cat.glm ,id.n = 2)
