# 
#--Building estimation model based on ARMA and VAR to see if  Oil Price affects CPI of Canada
#
#We  will build a (forecast)  model for Canada inflation starting from 1990 and till to ?????  and  extend it by adding one exogenous variable (Oil price)
#   
#
# first we install all  Packages and functions that we need
library(tsbox)
library(forecast)
library(quantmod)
library(xts)
library(ggplot2)   
library(seasonal)
library(CADFtest) 
library(reshape2)
library(vars)
library(stargazer)

# Delete all objects in the memory                                                                                   
rm(list=ls())

# Load user-defined commands and packages
source("UserPackages.R")

# Here, it executes the function and creates the folder in the current directory
mainDir <- getwd()
getwd()
outDir <- makeOutDir(mainDir, "/ResultsCanada_real006")

# --------------------------------------------------------------
#                     *** PART I ***
# --------------------------------------------------------------
# --------------------------------------------------------------
# 1) Import the necessary data and seasonally adjust
# --------------------------------------------------------------

CPI = ts_fred('CANCPIALLMINMEI')
CPI = xts(CPI[, 3], order.by = CPI[, 2])
CPI = ts_span(CPI, "1990-01-01", "2021-09-01")
CPI = final(seas(ts_ts(CPI)))


OIL = ts_fred('POILBREUSDM')
OIL = xts(OIL[, 3], order.by = OIL[, 2])
OIL = ts_ts(ts_span(OIL, "1990-01-01", "2021-09-01"))

USDCA = ts_fred('DEXCAUS')
USDCA = xts(USDCA[, 3], order.by = USDCA[, 2])
ts_frequency(USDCA,to = c("month"), aggregate="mean", na.rm=T)
USDCA= ts_frequency(USDCA,to = c("month"), aggregate="mean", na.rm=TRUE)
USDCA = ts_ts(ts_span(USDCA, "1990-01-01", "2021-09-01"))
print(USDCA)

# Convert USD oil price to oil price in terms of CA$
OIL = OIL/USDCA
OIL = ts_xts(OIL)


# --------------------------------------------------------------
# 2) Visual inspection of the data
# --------------------------------------------------------------
ts_plot(
  `CPI`= log(CPI),
  title = "CA CPI",
  subtitle = "Log(Index)"
)
ts_save(filename = paste(outDir, "/CPILevel.pdf", sep = ""), width = 8, height = 5, open = FALSE)



ts_plot(
  `CPI`= ts_pc(CPI),
  title = "CA CPI",
  subtitle = "Percentage growth to previous period (month-on-month)"
)
ts_save(filename = paste(outDir, "/CPImom.pdf", sep = ""), width = 8, height = 5, open = FALSE)

ts_plot(
  `CPI`= ts_pcy(CPI),
  title = "CA CPI",
  subtitle = "Percentage growth to same period previous year (year-on-year)"
)
ts_save(filename = paste(outDir, "/CPIyoy.pdf", sep = ""), width = 8, height = 5, open = FALSE)


# --------------------------------------------------------------
# 3) Unit root tests
# --------------------------------------------------------------

# We perform an augmented Dickey Fuller test (ADF) to test whether the series contains a unit root

uRootCPI = CADFtest(log(CPI), max.lag.y = 10, type = "trend", criterion = "BIC")
summary(uRootCPI)
# We get very high p-value = 0.24   (we don't take into account t-stat in this test). 
# Since TS is not covariance stationary we  try to take first difference of  the CPI
uRootCPId = CADFtest(ts_diff(log(CPI)), max.lag.y = 10, type = "drift", criterion = "BIC")
summary(uRootCPId)
# we got very small p-value. Now our data is covariance stationary.
# Let us do the same operation for Oil price. 
uRootOIL = CADFtest(log(OIL), max.lag.y = 10, type = "trend", criterion = "BIC")
summary(uRootOIL)
uRootOILd = CADFtest(ts_diff(log(OIL)), max.lag.y = 10, type = "drift", criterion = "BIC")
summary(uRootOILd)


# --------------------------------------------------------------
# 4) Select univariate forecasting model and inspect residuals
# --------------------------------------------------------------
difference_CPI = ts_diff(log(CPI))
# Select the lag order with an information criterion
# Now, we would like to estimate a range of possible AR and MA models
# and compare the information criteria (for p = 0,..,3 and q = 0,..,3)
maxP = 3
maxQ = 3
AIC = matrix(data=NA,nrow=maxP+1,ncol=maxQ+1)
BIC = matrix(data=NA,nrow=maxP+1,ncol=maxQ+1)
colnames(AIC) = 0:maxQ
colnames(BIC) = 0:maxQ
rownames(AIC) = 0:maxP
rownames(BIC) = 0:maxP

# Loop over all possible lag orders, estimate the model, and save the criteria in a matrix
# Find the lag order with the smallest information criterion (nameMin is a user-defined function that is useful
# to find the name of the columns and rows of the minimum value of a matrix)
# Loop over all possible lag orders, estimate the model, and save the criteria in a matrix
for (p in 0:maxP){
  for (q in 0:maxQ){
    
    # Estimate the corresponding model
    temp = Arima(difference_CPI, order = c(p, 0, q), include.constant= TRUE)
        
    # Save the information criterion
    AIC[p+1, q+1] = temp$aic
    BIC[p+1, q+1] = temp$bic
  }
}

# Find the lag order with the smallest information criterion (nameMin is a user-defined function that is useful
# to find the name of the columns and rows of the minimum value of a matrix)
minCritAIC = nameMin(AIC)
minCritBIC = nameMin(BIC)

print("Lag order according to AIC and BIC (p, q)")
minCritAIC
minCritBIC

# We can see that AIC favours (0,1) model and BIC favours (0,0) model.
# Since our goal is not directly focused on the forecast we can chose AIC minimum information criteria 
# Estimate the ARMA(0,1) model and produce a forecast
Model = Arima(difference_CPI, order = c(0, 0, 1), include.constant= TRUE)
summary(Model)
checkresiduals(Model)+theme_minimal()
Box.test(Model$residuals, lag=12, fitdf=1, type="Lj")
# Interpretation:ARMA(0,1) model selected by BIC information crieria seem to fit our data very well.


# --------------------------------------------------------------
#                     *** PART II ***
# --------------------------------------------------------------
#
# --------------------------------------------------------------
# 7) Extend model to incorporate the oil price as exogenous regressor
# --------------------------------------------------------------

difference_OIL = ts_diff(log(OIL))
ModelX = Arima(difference_CPI, order = c(0, 0, 1), include.constant= TRUE, xreg = ts_c(difference_OIL))
summary(ModelX)
checkresiduals(ModelX)+theme_minimal()

# Interpretation: The residuals do not look much better than before. The oil price is significant and has a positive impact on CPI inflation.
# We now can ask how CPI inflation will develop under different assumptions for the future crude oil price. 


# -------------------------------------------------------------
# 8) VAR model 
# --------------------------------------------------------------
# Estimate  VAR model, using system of equation to estimate Vector AutoRegressive Model. Here we will have one equation for each endogenuous variable
#download necessary datas and transfrom them into necessary time span.

# 1: check for stationary
CPI = ts_fred('CANCPIALLMINMEI')
CPI = xts(CPI[, 3], order.by = CPI[, 2])
CPI = ts_span(CPI, "2000-01-01", "2021-09-01")
CPI = final(seas(ts_ts(CPI)))


OIL = ts_fred('POILBREUSDM')
OIL = xts(OIL[, 3], order.by = OIL[, 2])
OIL = ts_ts(ts_span(OIL, "2000-01-01", "2021-09-01"))

USDCA = ts_fred('DEXCAUS')
USDCA = xts(USDCA[, 3], order.by = USDCA[, 2])
ts_frequency(USDCA,to = c("month"), aggregate="mean", na.rm=T)
USDCA= ts_frequency(USDCA,to = c("month"), aggregate="mean", na.rm=TRUE)
USDCA = ts_ts(ts_span(USDCA, "1990-01-01", "2021-09-01"))
print(USDCA)
# Convert USD oil price to oil price in terms of CA$
OIL = OIL/USDCA

# autoplot data to check for ...
autoplot(CPI)
autoplot(OIL)
# We kind of repeat the same thing we did in part 1, 
# We perform an augmented Dickey FUller test (ADF) to test whether the series contains a unit root
# Because both series have an upward trend, we add a trend as a deterministic component.
# The lags of the regression are selected by the BIC. Recall that the null hypothesis is that the series
# contains a unit root (is non-stationary)
uRootCPI = CADFtest(log(CPI), max.lag.y = 10, type = "trend", criterion = "BIC")
summary(uRootCPI)
# I get very high p-value = 0.24   (we don't look at t-stat as usual here (-2.6)), now I try to take first difference of CPI
uRootCPId = CADFtest(ts_diff(log(CPI)), max.lag.y = 10, type = "drift", criterion = "BIC")
summary(uRootCPId)
# we got very small p-value
# same we do for OIl and the results are similar.
uRootOIL = CADFtest(log(OIL), max.lag.y = 10, type = "trend", criterion = "BIC")
summary(uRootOIL)

uRootOILd = CADFtest(ts_diff(log(OIL)), max.lag.y = 10, type = "drift", criterion = "BIC")
summary(uRootOIL)

# remember H0: is saying series is not stationary  and in by taking first difference we reject the Null hypothesis since our p-value is very small. 
dif_Oil=ts_diff(log(OIL))
autoplot(dif_Oil)
print(dif_Oil)

dif_CPI= ts_diff(log(CPI))
autoplot(dif_CPI)


#2: choose Optimal lag length based on "information criteria" in VAR model 

y=data.frame(dif_CPI,dif_Oil)
y = y[-1,]
# rownames(y) <- 1:nrow(y)
print(y)

lag = VARselect(y,lag.max=4)# without max the default lag is ten
#lag= Var select(y)
lag
#All criteria favored  to chose 1 lag
# If I just want to print the selection by information criteria 
lag$selection


#3: estimate the VAR model (if we have 2 endogenous variables, CPI and Oil we have 2 equations, one is for CPI and one for OIL price)
# we use OLS, it goes equation by equation. We put lag length = 1 because information criteria sugested us only one lag length. When we plot our data after 1st difference there is no more time trend so, we don't include trend in the estimation.
#
estim =VAR(y, p=1, type= "none")
summary(estim)
# stargazer would give us a nice table presentation
stargazer(estim[["varresult"]], type='text')
# how to save it as an image?

# 4: Now we check if the  model is stable
# Eigenvalues should be less than 1 if our model is stable
# We need these to be less than 1 to have stable model and indeed they are less than 1:: [1] 0.3210543 0.3210543
roots(estim,modulus =TRUE)
# make sure mdel is stable, egan vasier is smaller than 1 , if stationary it is less than 1. We can choose the lag max=4 for example 

# We can use VAR for forecasting 
fcstVAR = predict (estim, h.ahead=36)
fanchart (fcstVAR, cis= c(.5,.8, 0.95), plot.type= "single")
fcstVAR$fcst$CPI

# 5: Now we will do Granger Causality test. This test tells us which variable causes which variable  
# Granger causality test (whether y1 granger cause y2, whether y2 granger cause y1)

grangerCPI= causality(estim, cause= "dif_CPI")
#whether CPI causes OIL pirce
grangerCPI$Granger
# data:  VAR object estim
#F-Test = 3.8256, df1 = 1, df2 = 514, p-value = 0.05102
#Hypothesis test H0: CPI does not Granger cause OIL price. And our p-value is 5% so we  fail to reject it . It does not cause OIL price.
grangerOIL= causality(estim, cause= "dif_Oil")
#whether OIL causes CPI 
grangerOIL$Granger
#data:  VAR object estim
#F-Test = 23.351, df1 = 1, df2 = 514, p-value = 1.785e-06
# In this case p-value is very small, so we reject the Null and indeed Oil price cause CPI 


#6:  Generate Impulse Response Functions

#IFRs
# here we want to say which variable to shock, we said here impulse come from CPI and we want to see the response of OIL price
# with "n.ahead" we can tell how many steps ahead we want this to tell us the response (we check n head= 20 month ahead)
IRF1 = irf(estim, impulse= "dif_CPI", response= "dif_Oil",
           n.ahead= 20, boot=TRUE, run=200, ci=0.95)
plot(IRF1, ylab= "dif_Oil", main= "OIL price response to CPI shock")


IRF1 = irf(estim, impulse= "dif_Oil", response= "dif_CPI",
           n.ahead= 20, boot=TRUE, run=200, ci=0.95)
plot(IRF1, ylab= "dif_CPI", main= "CPI response to Oil price shock")















