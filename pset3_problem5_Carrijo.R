
rm(list =ls())

# problem set 3 Exercise 5 

# loading the package
library(haven)

#importing the dataset
JEC <- read_dta("C:/Users/mathe/OneDrive - Fundacao Getulio Vargas - FGV/Matérias/Econometria/JEC.dta")

#visualisation of the data

View(JEC)

##################################

# estimating the regression

##################################

data_red <- data.frame(log(JEC$price), log(JEC$quantity), JEC[,5:16])

names(data_red)[1:2] = c("ln_price", "ln_quantity")

reg_ols <-  lm(ln_price ~ . , data = data_red)


# summaring the result 

summary(reg_ols)

# the coefficient and standart error 
# the results is -0252 to the coefficient and the standart error is 0.032
print(summary(reg_ols)$coefficients[2,1:2])


# exercise c)

# the package Applied econometrics with R (AER) contains the function to estimate IV regresion
library(AER)

# the will use cartel as instrumento to estimate the elasticity of price in the previous regression

# including the cartel data on the data set

data_red$cartel <- JEC$cartel

# estimating the IV regression

# a way to abreviate the regression 

# there is two ways of writing the formula 
# sintetic way 

reg_iv <-ivreg(ln_price ~ ln_quantity + . - cartel | . -ln_quantity + cartel ,data = data_red)


# complete 
#reg_iv_2 <- ivreg(ln_price ~  seas1 + seas2 + seas3 +seas4 + seas5 + seas6 + seas7 + 
#                            seas8 + seas9 +seas11 + seas12 + ln_quantity | 
#                  seas1 + seas2 + seas3 +seas4 + seas5 + seas6 + seas7 + 
#                  seas8 + seas9 +seas11 + seas12 + cartel, data = data_red)

summary(reg_iv)

# two stage least square

second_est <- lm(ln_quantity ~ cartel + . - ln_price, data = data_red)

reduced <- lm(ln_price ~second_est$fitted.values + . -ln_quantity - cartel , data = data_red)

summary(reduced)

# diferrence between the ols estimatin and instrument variable estimation, both with robust std error
parameters <- rbind(summary(reg_ols, vcov = vcovHAC(reg_ols))$coefficients[2,1:2],
              summary(reg_iv, vcov = vcovHAC(reg_iv))$coefficients[2,1:2])

row.names(parameters) <- c("reg_ols", "reg_iv")

parameters

##############################################################################
# testing the hypothesis of b_1 =1

linearHypothesis(model = reg_ols,x,h, vcov. = vcov(reg_ols))

h = 1 
x <- rep(0,15)
x[2] = 1


# other way of testing 

linearHypothesis(model = reg_elas, c('ln_quantity = 1'), white.adjust = 'hc1')
linearHypothesis(model = reg_elas, c('ln_quantity = -1'), white.adjust = 'hc1')

