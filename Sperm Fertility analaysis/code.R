#some pre-defined functions

df=function(x){
  d=df.residual(x)
  return(d)
}
G=function(x){
  g=deviance(x)
  return(g)
}
p=function(x){
  P=pchisq(x$deviance,df=x$df.residual,lower.tail=F)
  return(P)
}
#Reading the data
fertility=read.csv("C:\\Users\\manub\\Downloads\\CDA\\project\\fertility.csv")
summary(fertility)

#renaming the columns
colnames(fertility)=c("S","A","CD","T","O","F","AC","C","H","D")

#independent model
fertility_ind1<- glm(data=fertility,formula=D~.,family = binomial(link = "logit"))
summary(fertility_ind1)

#variable selection
variable_selec=step(fertility_ind1, data=fertility,scope = list(upper=fertility_ind1,lower=fertility_ind2),direction = "back")
summary(variable_selec)

#Completely independent model
fertility_ind=glm(data=fertility,formula=D~S+T+AC,family=binomial(link="logit"))
summary(fertility_ind)

#Joint independence
joint_ind1=glm(data=fertility,formula=D~S+T*AC,family=binomial(link="logit"))
summary(joint_ind1)

joint_ind2=glm(data=fertility,formula=D~S*T+AC,family=binomial(link="logit"))
summary(joint_ind2)

joint_ind3=glm(data=fertility,formula=D~S*AC+T,family=binomial(link="logit"))
summary(joint_ind3)

#conditional independence
cond_ind1=glm(data=fertility,formula=D~S*AC+T*AC,family=binomial(link="logit"))
summary(cond_ind1)

cond_ind2=glm(data=fertility,formula=D~S*T+AC*T,family=binomial(link="logit"))
summary(cond_ind2)

cond_ind3=glm(data=fertility,formula=D~S*T+S*AC,family=binomial(link="logit"))
summary(cond_ind3)

#homogeneous
hom=glm(data=fertility,formula=D~S*T+S*AC+T*AC,family=binomial(link="logit"))
summary(hom)

#saturated model
sat=glm(data=fertility,formula=D~S*T*AC*F,family=binomial(link="logit"))
summary(sat)

#creating a dataframe for results
results=data.frame(model=c("(S, T, AC) ","(S, TAC)","(ST, AC)","(SAC, T)","(SAC, TAC)","(ST, ACT) ","(ST, SAC) "," (ST, SAC, TAC)"),Df=c(df(fertility_ind),df(joint_ind1),df(joint_ind2),df(joint_ind3),df(cond_ind1),df(cond_ind2),df(cond_ind3),df(hom)),G_2=c(G(fertility_ind),G(joint_ind1),G(joint_ind2),G(joint_ind3),G(cond_ind1),G(cond_ind2),G(cond_ind3),G(hom)),P=c(p(fertility_ind),p(joint_ind1),p(joint_ind2),p(joint_ind3),p(cond_ind1),p(cond_ind2),p(cond_ind3),p(hom)),AIC=c(AIC(fertility_ind),AIC(joint_ind1),AIC(joint_ind2),AIC(joint_ind3),AIC(cond_ind1),AIC(cond_ind2),AIC(cond_ind3),AIC(hom)))

#residuals
h=hatvalues(joint_ind3)
pearson=residuals(joint_ind3,type="pearson")
adjusted=pearson/sqrt(1-h)


