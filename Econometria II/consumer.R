library(quantreg)
library(sandwich)
library(lmtest)
library(stargazer)

consumer<-read.csv('consumer.csv')

equation<-PX1~RATEX+UNEMP+AGE+SEX+FAMSIZE+INCOME

# Question a)

ols<-lm(equation, data=consumer)
coeftest(ols, vcov=vcovHC)

# Qustion b)

tau<-c(.25, .5, .75)
qr<-rq(equation, data=consumer, tau=tau)
summary(qr, se='nid')

# Tables

stargazer(coeftest(ols, vcov=vcovHC))
stargazer(summary(qr, se='nid')[[1]][3])
stargazer(summary(qr, se='nid')[[2]][3])
stargazer(summary(qr, se='nid')[[3]][3])
