# install.packages("lmtest")
# install.packages("car")
install.packages("ztools")
library(lmtest)
library(car)
# load the employee.csv file from data folder
getwd()
employee <- read.csv("data/employee.csv")
head(employee)


mul_emp <- lm(salary ~ jobtime + salbegin + prevexp, data = employee)
mul_emp
summary(mul_emp)

library(dplyr)
wage <- read_excel("data/wage.xls")
head(wage)

# regression
reg_wage <- lm(wage ~ female + nonwhite + union + education + exper, data = wage)
reg_wage
summary(reg_wage)
# confidence interval
confint(reg_wage)

reg_wage1 <- lm(wage ~ female + nonwhite + union + education, data = wage)
summary(reg_wage1)

library("jtools")
# install.packages("jtools")
# compare the two models
# export_summs(reg_wage, reg_wage1, model.names = c("Model with Experience", "Model without Experience"))

anova(reg_wage, reg_wage1)
AIC(reg_wage, reg_wage1)
BIC(reg_wage, reg_wage1)

library(car)
outlierTest(reg_wage)

qqPlot(reg_wage, main = "QQ Plot for Wage Regression")
qqPlot(reg_wage, id.method = "identity", main = "QQ Plot with Identity Labels", simulate = TRUE)

# Check multicollinearity
vif(reg_wage)
residualPlot(reg_wage, main = "Residual Plot for Wage Regression")


# Data file
install.packages("foreign")
library(foreign)
cobb <- read.dta("data/cobb.dta")
head(cobb)

dim(cobb)
summary(cobb)
str(cobb)

cobb_model <- lm(log(output) ~ log(labor) + log(capital), data = cobb)
summary(cobb_model)
