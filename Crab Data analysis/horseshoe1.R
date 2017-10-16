#1
horseshoe=read.csv("C:\\Users\\manub\\Downloads\\CDA\\horseshoe.csv")
meaan=mean(horseshoe$Sa)
variance=var(horseshoe$Sa)
plot(horseshoe$Sa)
summary(horseshoe$Sa)
hist(horseshoe$Sa,main = "Histogram of number of Saites",xlab = "number of Saites")
plot(horseshoe$Sa,horseshoe$Wt,main = "Relationship between Wt and number of Satellites",xlab = "number of Saites",ylab = "Weight")
#plot(horseshoe$Wt,log(horseshoe$Sa))
#2a
x=horseshoe$Wt
y=horseshoe$Sa
data.model=glm(formula = y ~ x, family = poisson(link = log))
summary(data.model)
predict.log<-predict(data.model,type = "response")
plot(x,y,col="red")
abline(-0.4282,0.5892,col="blue")
#2b
#Estimating the mean of Y for female horseshoes of average Wt 2.44 kg
u=exp(-0.4282)*exp(0.5892*2.44)
#2c
ui=0.5892+(1.96*0.0650)
li=0.5892-(1.96*0.0650)
#2d
library("aod")
wald=wald.test(b = coef(data.model), Sigma = vcov(data.model),Terms = 2:2)
#2e
library(lmtest)
likelihood=lrtest(data.model)
#3a
library(MASS)
data.nb = glm.nb(data =horseshoe, formula = Sa ~ Wt, link = log)
#3b
ui1=0.7599+(1.96*0.1578)
li1=0.7599-(1.96*0.1578)
#4
library(pscl)
data.zero = zeroinfl(Sa ~ Wt | W, data = horseshoe)
fitted_values <- data.frame(Given = horseshoe$Sa,Poisson = data.model$fitted.values,Negative_binomial = data.nb$fitted.values,Zero_inflated = data.zero$fitted.values)
ggplot(data = horseshoe, aes(x= Sa)) + geom_histogram(binwidth = 1) 
ggplot(data = horseshoe, aes(x= poisson)) + geom_histogram(binwidth = 1)
ggplot(data = horseshoe, aes(x= nb)) + geom_histogram(binwidth = 1)

fitted= fitted_values %>% gather(type, value , Given:Zero_inflated)
ggplot(fitted, aes(value, colour = type)) +  geom_density()
ggplot(data = fitted, aes(x= value)) + geom_histogram(binwidth = 1)  

ggplot(data = fitted, aes(x= value)) + geom_histogram(binwidth = 1)+ facet_wrap(~type, ncol = 2) 

hist(horseshoe$Sa) + lines(density(horseshoe$poisson))