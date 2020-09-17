# set local directory to where data set is 
# setwd("C:/Users/Pedro/Dropbox/ecoiii2020/Lecture7_var_vec/VEC/")

# Load package using a function load_package-----------------------------------------------------------------
load_package<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

load_package('tseries')
load_package('urca')
load_package('dlm')
load_package('openxlsx')
load_package('xts')
load_package('class')
load_package('zoo')
load_package('fBasics')
load_package('qrmtools')
load_package('stats')
load_package('MTS')
load_package('vars')
load_package('graphics')
load_package('readxl')



##
# read S&P500 spot and fut already transformed in logs
##

sp500 <- read_excel("C:/Users/Pedro/Dropbox/ecoiii2020/Lecture7_var_vec/VEC/sp500.xls")

##
# transform in time series 
##
 sp500_spot=ts(sp500$SPOT)
 sp500_fut=ts(sp500$FUT)
 
##
# plot the two series
##
 par(mfrow=c(1,2))
 plot(sp500_spot,type='l', col='blue', main = 'Spot of log(SP500)')
 plot(sp500_fut,type='l', col='red', main = 'Fut of log(SP500)')
 

 
 par(mfrow=c(1,1))
 plot.default(sp500_spot, sp500_fut, col='darkblue')
 regressao <- lm(sp500_spot ~ sp500_fut)
 summary(regressao)
 abline(coef(regressao), col='red')
 
 legend('topleft', legend=c('sp500_spot=0.2066883+0.9661331*sp500_fut'), col=c('red'), pch=15)
 title('Scatter Plot - least squared fitted') 

 
 ##
 # define the Equilibrium correction by sp500_spot-a-b*sp500_fut
 ##
 EqCM=sp500_spot-regressao$coefficients[1]-regressao$coefficients[2]*sp500_fut
 
 ##
 # plot of EqCM
 ##
 plot(EqCM,type='l', col='steelblue', main = 'Equilibrium correction using OLS')
 
 
 ##
 # ACF for EqCm
 #
 par(mfrow=c(2,1))
 acf(EqCM, lag.max=12)
 pacf(EqCM, lag.max=12)
 
 ##
 # test unit roots in EqCM
 ##
 EqCM_adf = adf.test(EqCM)
 EqCM_adf
 
 
 ##
 # Generate EqCM with beta = (1,-1)
 ##
 EqCM11 = sp500_spot-sp500_fut
 
 ##
 # plot of EqCM
 ##
 par(mfrow=c(1,1))
 plot(EqCM11,type='l', col='steelblue', main = 'Equilibrium correction using beta = (1,-1)')
 
 
 ##
 # ACF for EqCm
 #
 par(mfrow=c(2,1))
 acf(EqCM11, lag.max=12)
 pacf(EqCM11, lag.max=12)
 
 ##
 # test unit roots in EqCM11
 ##
 EqCM11_adf = adf.test(EqCM11)
 EqCM11_adf
 
 ##
 # EqCM as the residual for the static regression of sp500_spot into constant and sp500_fut
 ##
 EqCM_regressao <- lm(sp500_spot ~ sp500_fut)
 summary(EqCM_regressao)
 EqCM_res=ts(EqCM_regressao$residuals)
 
 ##
 # plot of EqCM_res
 ##
 par(mfrow=c(1,1))
 plot(EqCM_res,type='l', col='steelblue', main = 'Equilibrium correction using beta from OLS')
 
 
 ##
 # ACF and PACFfor EqCm_res
 #
 par(mfrow=c(2,1))
 acf(EqCM_res, lag.max=12)
 pacf(EqCM_res, lag.max=12)
 
 
 ##
 # test unit roots in EqCM_res
 ##
 EqCM_res_adf = adf.test(EqCM_res)
 EqCM_res_adf
 

 
##
# lag one period transforme sp500_spot and sp500_fut 
##
sp500_spot_1=ts(c(NA,sp500_spot[1:length(sp500_spot)-1]))
sp500_fut_1=ts(c(NA,sp500_fut[1:length(sp500_fut)-1]))


##
# transforme sp500_spot and sp500_fut in first difference
##
dsp500_spot = sp500_spot-sp500_spot_1
dsp500_fut = sp500_fut-sp500_fut_1

##
# lag one period  dsp500_spot and dsp500_fut 
##
dsp500_spot_1=ts(c(NA,dsp500_spot[1:length(dsp500_spot)-1]))
dsp500_fut_1=ts(c(NA,dsp500_fut[1:length(dsp500_fut)-1]))

##
# lag one period EqCM_res
##
EqCM_res_1=ts(c(NA,EqCM_res[1:length(EqCM_res)-1]))

##
# equation for dsp500_spot(t) = a1 + b11*EqCM_res(t-1) + b12* dsp500_spot(t-1) + b13*dsp500_fut(t-1)
##
eq_spot <- lm(dsp500_spot ~ EqCM_res_1 + dsp500_spot_1 +  dsp500_fut_1)
summary(eq_spot)


##
# equation for dsp500_fut(t) = a2 + b21*EqCM_res(t-1) + b22* dsp500_spot(t-1) + b23*dsp500_fut(t-1)
##
eq_fut <- lm(dsp500_fut ~ EqCM_res_1 + dsp500_spot_1 +  dsp500_fut_1)
summary(eq_fut)



##
# sp500 only spot and fut 
##
sp500_spot_fut=ts(sp500[,3:2])



##
# VAR lag length selection in VARS
##

VARselect(sp500_spot_fut,lag.max=20, type=c("const")) 

##
# best model is VAR(20) using AIC, VAR(6) using BIC and VAR(8) using HQ
##

##
# we are going to use VAR(20)
##

VAR_spot_fut20 <-VAR(sp500_spot_fut , p=20, type = "const")

VAR_spot_fut20

##
# summary of equation spot
##
summary(VAR_spot_fut20, equation = "SPOT")
##
# plot fit and residuals and ACF and PACF for residuals for SPOT
##
plot(VAR_spot_fut20, names = "SPOT")

##
# summary of equation fut
##
summary(VAR_spot_fut20, equation = "FUT")
##
# plot fit and residuals and ACF and PACF for residuals for FUT
##
plot(VAR_spot_fut20, names = "FUT")


##
#  Portmanteau test for system 
##
ser20 <- serial.test(VAR_spot_fut20, lags.pt = 20, type = "PT.asymptotic")
ser20

##
# JB Test for system
##
JB20 <- normality.test(VAR_spot_fut20)
JB20$jb.mul

##
# ARCH test for system
##
arch20 <- arch.test(VAR_spot_fut20, lags.multi = 20)
arch20$arch.mul


##
# diagnostic for spot equation
##
plot(arch20,name= "SPOT")
plot(stability(VAR_spot_fut20),nc=2)


plot(arch20,name= "FUT")


##
# Johansen procedure for SPOT and FUT
##

vec_spot_fut_eigen <- (ca.jo(sp500_spot_fut, type=c("eigen"), ecdet = c("const"), K=19, spec = c("transitory")))
summary(vec_spot_fut_eigen)

vec_spot_fut_trace <- (ca.jo(sp500_spot_fut, type=c("trace"), ecdet = c("const"), K=19, spec = c("transitory")))
summary(vec_spot_fut_trace)

