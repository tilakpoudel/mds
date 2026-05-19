###Logistic Regression

##Open employee data;


###Making dummies

###First method
employee$gender_m<-ifelse(employee$gender=="m", 1,0)
head(employee)

###Question :Create the two dummies for employee
employee$jobcat_A<-ifelse(employee$jobcat==1, 1, 0)
head(employee)
###Second method by using packages
# Install the required package
install.packages("fastDummies")

# Load the library
library(fastDummies)
employee <- dummy_cols(employee, select_columns = "jobcat")
head(employee)

sal_m<-lm(employee$salary~employee$gender_m, employee)
summary(sal_m)
coefficients(sal_m)
sal_female =26031.92
sal_male=26031.92+15409.86
sal_male

sal_jobcat<-lm(salary~jobcat_1+jobcat_2, employee)
summary(sal_jobcat)
coefficients(sal_jobcat)
sal_pred<-list(jobcat_1=c(1,0,0),jobcat_2=c(0,1,0)) 
predict(sal_jobcat,sal_pred)

##mean comparison
employee %>% 
  group_by(jobcat) %>%    
  summarize(mean = mean(salary),  n = length(salary), sd = sd(salary))            

##Open MROZ.dta data file
head(MROZ)
linprob<-lm(inlf~nwifeinc+educ+exper+I(exper)^2+age+kidslt6+kidsge6, data=MROZ)
summary(linprob)

###for prediction.
pred<-list(nwifeinc=c(100,0),educ=c(5,15), exper=c(0,10),age= c(20,52),kidslt6=c(2,0), kidsge6=c(0,0))
predict(linprob,pred)   

###Logistic regression
glm(formula=
inlf~nwifeinc+educ+exper+I(exper^2)+age+kidslt6+kidsge6,family=binomial(link=logit), data=MROZ)

logit_femp<- glm(formula=inlf~nwifeinc+educ+exper+I(exper^2)+age+kidslt6+kidsge6,family=binomial(link=logit), data=MROZ)
summary(logit_femp)

##Odds Ratio
# Logit model odds ratios
exp(logit_femp$coefficients)

###Log likelihood
logLik(logit_femp)

###McFadden's pseudo R2
1-logit_femp$deviance/logit_femp$null.deviance

lrtest(logit_femp)


##Probit Reg
probit_femp<- glm(formula=inlf~nwifeinc+educ+exper+I(exper)^2+age+kidslt6+kidsge6,family=binomial(link=probit), data=MROZ)
summary(probit_femp)

###Log likelihood in probit
logLik(probit_femp)

###McFadden's pseudo R2 in probit
1-probit_femp$deviance/probit_femp$null.deviance

library(lmtest)
lrtest(probit_femp)

##Smoking (Damoder Gujrati)
head(Smoking)
logit_smoke<- glm(smoker~age+educ+income+pcigs79,family=binomial(link=logit), data=Smoking)
summary(logit_smoke)

##Odds Ratio

exp(logit_smoke$coefficients)
