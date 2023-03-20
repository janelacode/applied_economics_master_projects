
#------------ The Impact of female ownership on firm performance in EU countries--------------------------------------------------------------

#------------Preparation of the data and libraries-------------------------------------------------------------------------------------------- 

# set directory
getwd()

# download necessary libraries
install.packages("psych")
require(spych)
library(psych)
library(stargazer)
library(lmtest)
library(zoo)
library(sandwich)
library(olsrr)
library(car)

# download our data that we cleared and prepared on EXCEL
data=read.csv("data.csv", header= TRUE, sep=";")


#------------------------Summary Statistics--------------------------------------------------------------------------------------------------

# bellow, we have the summary statistics of the variables of interest

stargazer(data, type="text", title="Descriptive statistics", digits=1, out="table1.txt")       
          
#-----------------------------------------test if our depedent variable is normaly distributed-----------------------------------------------
# plot dependent variable 
hist(data$ln_sales)


#----------------------First, we run our 4 models with conventional standard errors----------------------------------------------------------


# In model1, we run a regression model to examine the effect of being a female to manager on firm's sales
m1<- lm(ln_sales~top_manager_female+firm_age +manager_experience +firm_size+exportsDV+fixed_asset+product_innovation +manufacturing + retail+other_services+food + garments+ metal_products+machinery+ Cyprus+Estonia+Greece+Italy+Latvia+ Lithuania+ Malta+Portugal+Slovak_rep, data=data )
# In model2, we run a regression model to examine the effect of female owner on firm's sales
m2<- lm(ln_sales~female_owners+firm_age +manager_experience +firm_size+exportsDV+fixed_asset+product_innovation +manufacturing + retail+other_services+food + garments+ metal_products+machinery+ Cyprus+Estonia+Greece+Italy+Latvia+ Lithuania+ Malta+Portugal+Slovak_rep, data=data)

# create family data
family<-data[data$family_firms==1,]

#in model3 and model4 we have the same regressors but in family firm and not on the full sample.
#model3
m3<-lm(ln_sales~top_manager_female+firm_age +manager_experience +firm_size+exportsDV+fixed_asset+product_innovation +manufacturing + retail+other_services+food + garments+ metal_products+machinery+ Cyprus+Estonia+Greece+Italy+Latvia+ Lithuania+ Malta+Portugal+Slovak_rep, data=family)

#model4

m4<-lm(ln_sales~female_owners+firm_age +manager_experience+firm_size+ exportsDV+fixed_asset+product_innovation+manufacturing+retail+other_services+food+garments+metal_products+machinery+Cyprus+Estonia+Greece+Italy+Latvia+Lithuania+Malta+Portugal+Slovak_rep,data=family)

# with help of stargazer library, we can have a nice output of the results  

stargazer(m1,m2,m3,m4, type="text",
          dep.var.labels=c("log Sales"),
          covariate.labels=c("top manager female", "female owner","firm age","manager experience","firm size","exportsDV","fixed asset","product innovation","manufacturing","retail","other services","food","garments","metal products","machinery","Cyprus","Estonia","Greece","Italy","Latvia","Lithuania","Malta","Portugal","Slovak rep"),out="4_modelss.txt")


# The interpretations of the regression models can be found in the pdf file

# -------------------variance inflation factor, test for multicollinearity-------------------------------------------------------------------

m1<- lm(ln_sales~top_manager_female+firm_age +manager_experience +firm_size+exportsDV+fixed_asset+product_innovation +manufacturing + retail+other_services+food + garments+ metal_products+machinery+ Cyprus+Estonia+Greece+Italy+Latvia+ Lithuania+ Malta+Portugal+Slovak_rep, data=data )
vif(m1)



#----------------------------------- We run a Breusch-Pagan test for heteroskedasticity. H0: no heteroscedasticity in error terms------------

bptest(m1) # p-value < 2.2e-16, we reject the Null
bptest(m2) # p-value < 2.2e-16, we reject the Null
bptest(m3) # p-value < 2.2e-16, we reject the Null
bptest(m4) # p-value < 2.2e-16, we reject the Null


#------------------------------------We will use robust standard errors for each model ------------------------------------------------------
coeftest(m1, vcov = vcovHC(m1, type="HC1"))
coeftest(m2, vcov = vcovHC(m2, type="HC1"))
coeftest(m3, vcov = vcovHC(m3, type="HC1"))
coeftest(m4, vcov = vcovHC(m4, type="HC1"))


#------------------------------------we look at the results with robust standard errors------------------------------------------------------

model1=coeftest(m1, vcov = vcovHC(m1, type="HC1"))
model2=coeftest(m2, vcov = vcovHC(m2, type="HC1"))
model3=coeftest(m3, vcov = vcovHC(m3, type="HC1"))
model4=coeftest(m4, vcov = vcovHC(m4, type="HC1"))
stargazer(model1,model2,model3,model4, type="text",
          dep.var.labels=c("Sales"),
          covariate.labels=c("top manager female", "female owner","firm age","manager experience","firm size","exportsDV","fixed asset","product innovation","manufacturing","retail","other services","food","garments","metal products","machinery","Cyprus","Estonia","Greece","Italy","Latvia","Lithuania","Malta","Portugal","Slovak rep"),out="models with robust standard errors.txt")



# ---------------------------------plot the residuals---------------------------------------------------------------------------------------

ols_plot_resid_qq(m1)
ols_plot_resid_qq(m2)
ols_plot_resid_qq(m3)
ols_plot_resid_qq(m4)

#---------------------------------test for normality ---------------------------------------------------------------------------------------

ols_test_normality(m1)
ols_test_normality(m2)
ols_test_normality(m3)
ols_test_normality(m4)






 
