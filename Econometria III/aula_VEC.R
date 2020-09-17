#' ## Aula VAR-VEC
#' 
#+ warnings = FALSE, include = FALSE
library(tidyverse)
library(cowplot)
library(tseries)
library(urca)
library(vars)

source("Econometria III/ts_fun.R")

df <- read_csv("Econometria III/y_sys5.csv")
ts <- df[, -1]

#' ## Exploratory Analisys
#' 
#' Plot the three series
df %>% 
  pivot_longer(cols = -X1, names_to = "serie", values_to = "value") %>% 
  ggplot(aes(X1, value, group = serie, color = serie)) +
  labs(x = "time") +
  geom_line() +
  theme_light()

#' None of the series look stationary. Take ADF tests for unit root
#' There is **no economic reasoning** underlying the data, we may include
#' a deterministic trend.
summary(ur.df(df$V1, type = "trend", lags = 12, selectlags = "BIC"))
summary(ur.df(df$V2, type = "trend", lags = 12, selectlags = "BIC"))
summary(ur.df(df$V3, type = "trend", lags = 12, selectlags = "BIC"))

#' We barely can reject the null hypothesis of a unit root in series V1 and V2,
#' when not specifying drift or trend in the ADF test.
#' If we specify drift or drift and trend, then those variables becomes 
#' integrated. The exception is V1, which we cannot reject the null hypothesis
#' at 1% significance level. Hence, we proceed with caution, but assuming all 
#' three variables are integrated. Now we have to check the order of integration
#' by differencing the series and doing the tests again.
summary(ur.df(diff(df$V1), type = "trend", lags = 12, selectlags = "BIC"))
summary(ur.df(diff(df$V2), type = "trend", lags = 12, selectlags = "BIC"))
summary(ur.df(diff(df$V3), type = "trend", lags = 12, selectlags = "BIC"))

#' On all three differenced series we were able to reject the null hypothesis of
#' a unit root. Thus, the variables are integrated of order 1.

#' Long-term regressions
fit1 <- lm(V1 ~ ., data = ts)
summary(fit1)
fit2 <- lm(V2~., data = ts)
fit3 <- lm(V3~., data = ts)

#' define the Equilibrium correction
EqCM1 <- resid(fit1)
attr(EqCM1, "names") <- NULL
EqCM2 <- resid(fit2)
attr(EqCM2, "names") <- NULL
EqCM3 <- resid(fit3)
attr(EqCM3, "names") <- NULL

#' plot of EqCM
plot(EqCM1,type='l', col='steelblue', main = 'Equilibrium correction using OLS')

#' ACF for EqCm
#+ warning = FALSE
ggplot.corr(data = EqCM1, lag.max = 12, large.sample.size = TRUE)

#' Test unit roots in EqCM
EqCM1_adf <- ur.df(EqCM1, type = "trend", lags = 12, selectlags = "BIC")
summary(EqCM1_adf)

#' The residuals from regression $V1_t=\mu+\beta_2 V2_t + \beta_3 V3_t + \varepsilon_t$
#' are stationary, although they seem to follow an AR(1) process, indicating 
#' there is a lack of dynamic structure in the above equation. Even though, we 
#' have a strong indication these series are cointegrated.
#' 
#' To be sure, let's test cointegration with Johansen's test.
#' 
#' ## Johansen's cointegration tests
#' 
#' Trend included
summary(ca.jo(ts, type = "eigen", ecdet = "trend", K = 20, spec = "transitory"))
summary(ca.jo(ts, type = "trace", ecdet = "trend", K = 20, spec = "transitory"))

#' Only drift included
summary(ca.jo(ts, type = "eigen", ecdet = "const", K = 20, spec = "transitory"))
summary(ca.jo(ts, type = "trace", ecdet = "const", K = 20, spec = "transitory"))

#' Neither drift or trend included
summary(ca.jo(ts, type = "eigen", ecdet = "none", K = 20, spec = "transitory"))
summary(ca.jo(ts, type = "trace", ecdet = "none", K = 20, spec = "transitory"))

#' Both Johansen tests, eigenvalue and trace, confirm there is at most two
#' cointegrating vectors for each of the three specifications. That is, there is
#' two linearly independent vectors
#' 
#' Now we will select the VAR order and check whether it is indeed a first order
#' process.
#' VAR lag length selection in VARS. 
#' There is no economic reasoning underlying the data, we may include
#' a deterministic trend.
VARselect(ts, lag.max = 20, type = c("both")) 
VARselect(ts, lag.max = 20, type = c("trend"))
VARselect(ts, lag.max = 20, type = c("const"))
VARselect(ts, lag.max = 20, type = c("none")) 

#' Selected order by all models and **all** information criteria was n=1
#' Among all specification with n=1 the one with lowest information criteria was
#' no constant and no trend
VARselect(ts, lag.max = 1, type = c("both")) 
VARselect(ts, lag.max = 1, type = c("trend"))
VARselect(ts, lag.max = 1, type = c("const"))
VARselect(ts, lag.max = 1, type = c("none")) 

#' Even though the theoretical best model is VAR(1) without either drift or 
#' linear trend, we will additionally specify models with only drift and both 
#' drift and trend to make comparisons
VAR_none_1 <- VAR(ts , p = 1, type = "none")
VAR_drift_1 <- VAR(ts , p = 1, type = "const")
VAR_both_1 <- VAR(ts , p = 1, type = "both")

summary(VAR_none_1, equation = "V1")
summary(VAR_drift_1, equation = "V1")
summary(VAR_both_1, equation = "V1")

#' Plot fit and residuals and ACF and PACF for residuals for the VAR(1) models
#' 
plot(VAR_none_1, names = "V1", 
     main.fit = "Fit and residuals for V1. Model: no drift or trend")
plot(VAR_drift_1, names = "V1",
     main.fit = "Fit and residuals for V1. Model: drift")
plot(VAR_both_1, names = "V1",
     main.fit = "Fit and residuals for V1. Model: drift and trend")

#' We barely see any difference in the plots!! Let's stick to the model without
#' a drift or trend. 
#' 
#' Let's do some residual tests for autocorrelation, normality, conditional 
#' heteroskedasticity and stability.
#' 
#'  Portmanteau test for residuals' autocorrelation.
ser <- serial.test(VAR_none_1, lags.pt = 20, type = "PT.asymptotic")
ser

#' Jarque-Bera Test to assess residuals' normality
##
jb <- normality.test(VAR_none_1)
jb$jb.mul

#' ARCH test for conditional heteroskedasticity. Null hypothesis is no serial 
#' (cross) autocorrelation up to max lag on the squared residuals.
arch <- arch.test(VAR_none_1, lags.multi = 20)
arch$arch.mul

#' Diagnostic plots. Residuals and squared residuals correlograms and CUMSUM
#' statiblity.
plot(arch, name = "V1")
plot(stability(VAR_none_1), nc = 2)

#' All tests look alright. We can, therefore, believe the given series form a 
#' cointegrated VAR(1) system without drift or tred, whose equations are given by:
#' 
summary(VAR_none_1)

#' ## Specifying a VECM
#' 
#' We will specify a **restricted** VECM without
#' any deterministic component. Since our VAR model is of order 1, the VEC model
#' will be zero order, that is, no lags of the differenced variables will be
#' included, leaving only the long term relation.
#' 
#' This long-term relation has already been estimated initially in our static
#' regression, thus:
#' 
#' $$\Delta y_t = \alpha\beta^\prime y_{t-1}+\varepsilon_t$$
#' 
#' We can estimate the three relations one-by-one with OLS on each variable. For
#' example: $\Delta V1_t = EqCMi_{t-1}\gamma + u1_t$, where EqCMi is the residuals
#' from the static regression of Vi against the other two variables.
#' 
#' Includes EqCM in the data frame, then lag one period, first difference, 
#' lag one period the first diff.
#' Removes the NAs leaving a sample size of 999
#' 
aug_df <- df %>% 
  mutate(EqCM1 = EqCM1,
         EqCM2 = EqCM2,
         EqCM3 = EqCM3,
         across(V1:EqCM3, lag, .names = "{col}_1"),
         across(V1:EqCM3, ~(.x - lag(.x)), .names = "d_{col}")) %>% 
  na.omit()

#' Regression for first equation in VECM
#' 
vec_fit_1 <- lm(d_V1 ~ EqCM1_1+EqCM2_1+EqCM3_1, data = aug_df)
vec_fit_2 <- lm(d_V2 ~ EqCM1_1+EqCM2_1+EqCM3_1, data = aug_df)
vec_fit_3 <- lm(d_V3 ~ EqCM1_1+EqCM2_1+EqCM3_1, data = aug_df)
summary(vec_fit_1)
summary(vec_fit_2)
summary(vec_fit_3)
