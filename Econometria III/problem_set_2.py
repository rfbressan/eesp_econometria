# Econometrics III - Problem Set II
# Practical Questions

# Question 3

import numpy as np
import pandas as pd
from scipy.stats import norm
import statsmodels.api as sm
import statsmodels.tsa.api as tsa
import matplotlib.pyplot as plt
# Package to interface with World Bank API
import wbdata as wb
# Package to interface with Ipeadata API
import ipeadatapy
# arch package to perform unit root test
from arch.unitroot import PhillipsPerron
from arch.unitroot import KPSS

# Generate random errors and simulate series
nobs = 1025
T = 25
np.random.seed(123456)
u = norm.rvs(size = nobs)
v = norm.rvs(size = nobs)

x = np.zeros(nobs)
y = np.zeros(nobs)
x[0] = u[0]
y[0] = v[0]
for i in range(1, nobs):
    x[i] = x[i-1]+u[i]
    y[i] = y[i-1]+v[i]

# Take only the last T observations
x = x[-T:]
y = y[-T:]
# Plotting the series
fig, ax = plt.subplots(figsize = (10,6))
ax.plot(np.arange(T), x, label = "x_t")
ax.plot(np.arange(T), y, label = "y_t")
ax.legend()
plt.show()

# Run the regression and plot a summary
x_reg = sm.add_constant(x)
fit = sm.OLS(y, x_reg).fit()
# Save summary to tex file
f = open('3a.tex', 'w')
f.write(fit.summary().as_latex())
f.close()


# Creating a function to handle the simulations with specified sample size, number of 
# simulations and presence of drift or not.
def simulate(nsim, size, drift=False):
    #pass
    mu=1 if drift else 0
    beta = np.zeros(nsim)
    const = np.zeros(nsim)
    r2 = np.zeros(nsim)
    dw = np.zeros(nsim)
    tstat = np.zeros(nsim)

    for sim in range(nsim):
        u = norm.rvs(size = nobs)
        v = norm.rvs(size = nobs)

        x = np.zeros(nobs)
        y = np.zeros(nobs)
        x[0] = u[0]
        y[0] = v[0]
        for i in range(1, nobs):
            x[i] = mu + x[i-1]+u[i]
            y[i] = mu + y[i-1]+v[i]

        # Take only the last T observations
        x = x[-size:]
        y = y[-size:]
        # Run the regression and plot a summary
        x_reg = sm.add_constant(x)
        fit = sm.OLS(y, x_reg).fit()
        beta[sim] = fit.params[1]
        const[sim] = fit.params[0]
        r2[sim] = fit.rsquared
        dw[sim] = sm.stats.stattools.durbin_watson(fit.resid)
        tstat[sim] = fit.tvalues[1]
    
    return {'const':const, 'beta': beta, 'R2': r2, 'DW': dw, 'tstat': tstat}

# Function to plot and save histograms
def plot_histograms(df, prefix, order = ['tstat', 'beta', 'DW', 'R2']):
    # Ploting histogram for beta and tstat
    fig, ax = plt.subplots(figsize=(10,6))
    ax.hist(df[order[0]], 50, label = order[0])
    ax.hist(df[order[1]], 50, label = order[1])
    ax.legend()
    filename = prefix + '_beta_t_hist.pdf'
    plt.savefig(filename)
    
    # Ploting histogram for R2 and DW
    fig, ax = plt.subplots(figsize=(10,6))
    ax.hist(df[order[2]], 50, label = order[2])
    ax.hist(df[order[3]], 50, label = order[3], alpha = 0.7)
    ax.legend()
    filename = prefix + '_r2_dw_hist.pdf'
    plt.savefig(filename)


# Repeat 2000 times and store the coefficient
pars = pd.DataFrame(simulate(2000, 25))
# Plot histograms
plot_histograms(pars, '3b')
# How is the proportion of t-stat > 1.96 in absolute value?
ratio_t = np.sum(np.abs(pars['tstat'] > 1.96)) / len(pars)

# Changing sample size to T=100 and T=1000
pars100 = pd.DataFrame(simulate(2000, 100))
pars1k = pd.DataFrame(simulate(2000, 1000))
#Ploting histograms
plot_histograms(pars100, '3c100')
plot_histograms(pars1k, '3c1k', order=['tstat', 'beta', 'R2', 'DW'])
# How is the proportion of t-stat > 1.96 in absolute value?
ratio_t1k = np.sum(np.abs(pars1k['tstat'] > 1.96)) / len(pars1k)

# Adding drift
pars_d = pd.DataFrame(simulate(2000, 25, drift=True))
# Ploting histogram
plot_histograms(pars_d, '3d')

# Whats the mean value of beta?
pars_d['beta'].mean()
# How is the proportion of t-stat > 1.96 in absolute value?
ratio_td = np.sum(np.abs(pars_d['tstat'] > 1.96)) / len(pars_d)

# Code for World Bank
# Brazillian GDP NY.GDP.MKTP.PP.CD?end=2019&locations=BR&start=1990
# br_gdp_dic = wb.get_data("NY.GDP.MKTP.PP.CD", country="BR")
# br_gdp = wb.get_dataframe({"NY.GDP.MKTP.PP.CD": "gdp"}, country="BR")
# br_gdp = br_gdp.dropna().sort_index()
ipeadatapy.list_series('PIB - preços de mercado - índice')
gdp = ipeadatapy.timeseries('SCN10_PIB10')[['VALUE (-)']]
gdp.rename(columns={'VALUE (-)': 'value'}, inplace=True)
log_gdp = 100*np.log(gdp)
log_gdp["trend"] = range(1, len(log_gdp)+1)
x = sm.add_constant(log_gdp[["trend"]])
# Regression of GDP on time trend and constant
reg = sm.OLS(log_gdp['value'], x)
fit = reg.fit()
# Extract residuals and fitted values
residuals = fit.resid
fitted = fit.fittedvalues
# Plot Series and fitted values on top and residuals on bottom
fig, ax = plt.subplots(2, 1, figsize=(10, 6))
ax[0].plot(log_gdp.index, log_gdp['value'], label = "100 log(gdp)")
ax[0].plot(fitted, label = 'Fitted')
ax[1].plot(residuals, label = 'Residuals')
ax[0].legend()
ax[1].legend()
plt.savefig('4a_fit.pdf')
# Plot ACF and PACF for residuals
fig, ax = plt.subplots(2,1, figsize=(10,6))
sm.graphics.tsa.plot_acf(residuals.values, lags=12, zero=False, ax=ax[0])
sm.graphics.tsa.plot_pacf(residuals.values, lags=12, zero=False, ax=ax[1])
plt.savefig('4a_acf.pdf')

# Take an ADF test on residuals
adf=tsa.adfuller(residuals, 1, "nc")
print(f'ADF test rejected null at 5%?: {"TRUE" if adf[1]<0.05 else "FALSE"}')
# Since residuals were detrended and ADF test rejected the null, GDP is
# trend stationary in this case.

# DF tests on GDP
nc_adf = tsa.adfuller(log_gdp['value'], 8, regression="nc", autolag=None)
c_adf = tsa.adfuller(log_gdp['value'], 1, regression="c")
ct_adf = tsa.adfuller(log_gdp['value'], 1, regression="ct")

adf_dic = {'model': ["No constant no trend", "Constant", "Constant and trend"],
           'stat': [nc_adf[0], c_adf[0], ct_adf[0]],
           'pval': [nc_adf[1], c_adf[1], ct_adf[1]],
           'lags': [nc_adf[2], c_adf[2], ct_adf[2]]}

adf_tb = pd.DataFrame(adf_dic)
# Save table to tex file
f = open('4b.tex', 'w')
f.write(adf_tb.to_latex(index=False))
f.close()

# Phillips-Perron and KPSS tests
# statsmodels does not have PP test
# Using package arch from Kevin Sheppard
pp = PhillipsPerron(residuals)
pp.trend = 'ct'
f = open('4c_pp.tex', 'w')
f.write(pp.summary().as_latex())
f.close()

kpss = KPSS(residuals)
kpss.trend = 'ct'
f = open('4c_kpss.tex', 'w')
f.write(kpss.summary().as_latex())
f.close()
