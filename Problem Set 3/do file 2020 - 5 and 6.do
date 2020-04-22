* Do File - Ex 5 and 6 - 2020
* TA: Giovanni Di Pietra


* Ex 5
clear
cd "C:\Users\gidip\Dropbox\2020 - Econometrics\Psets\Pset3"
use "JEC.dta", clear


/* During the 1880s, a cartel known as the Joint Executive Committee (JEC) controlled the rail 
transport of grain from the Midwest to eastern cities in the United States. 
The cartel preceded the Sherman Antritrust Act of 1890, and it legally operated 
to increase the price of grain transportation above what would
have been the competitive price. From time to time, cheating by members of the cartel 
brought about a temporary collapse of the collusive price-setting agreement. 
In this exercise, you will use variations in supply associated with the cartel's collapses 
to estimate the elasticity of demand for rail transport of grain. 

We will use a data file JEC.dta that contains weekly observations on the rail shipping price and other factors from 1880 to 1886. 
So, our main goal is to estimate the elasticity of demand for rail shipping of grain. 
In particular, we wish to regress $ln(Q_i)$ where $Q_i$ is the total tonnage of grain shipped in week $i$, 
on $ln(P_i)$, where $P_i$ is the price of shipping a ton of grain by rail.

\begin{enumerate}

\item  Estimate the price elasticity of demand by using OLS to regress the 
log of the quantity of grain shipped on the log of the price and the full set of monthly 
binary indicators. What is the estimated value of the demand elasticity and its standard error?

\item Explain why the interaction of supply and demand could make the OLS 
estimator of the elasticity obtained in (a) biased.

\item Consider using the variable "cartel" as instrumental variable for $ln(P)$.
 
Use economic "reasoning" to argue whether cartel plausibly satisfies the two conditions for a valid instrument.

* E (c_i p_i) \neq 0 , c_i -> p_i
*/

reg l_P cartel seas*, rob

/*
\item Test, at $5\%$ significance level, 
the hypothesis that the demand for rail shipping of grain is inelastic. 

\item Test, at $5\%$ significance level, the hypothesis that 
the demand elasticity is equal to $-1$. What is the p-value of this test?

\item One of the reasons for price fluctuations was that the Great Lakes periodically froze,
 making shipping grain by boat impossible and temporarily increasing the demand for rail. 
 $Ice_i$ is a binary variable that is equal to 1 if the Great Lakes are not navigable because of ice. 
 Consider using the variable Ice as instrumental variable for $ln(P)$. 
 Use economic reasoning to argue whether Ice plausibly satisfies the two conditions for a valid instrument 
 to estimate demand elasticity for rail shipping of grain.

\end{enumerate}

*/

// (a)

describe
* we have more than 300 obs -> assume n->\infty?


gen l_P = ln(price)
gen l_Q = ln(quantity)

reg l_Q l_P seas*, robust

* See regression line twowayplot

* e = -.6620433
* sd(e) = .0754213 

// (b)

* Usual simultaneous equation,

// (c)
***

* We know that if the price drops, the cartel is likely to form - but also to restrain the QS.
* May be not a good IV?
* But also only affects supply - not the demand for grain!

* 1st stage
reg l_P cartel, robust
scalar gamma1 = _b[cartel]

* Reduced form?
reg l_Q cartel, robust
scalar phi1 = _b[cartel]

* Then:
scalar biv = phi1/gamma1
dis biv

// (d)

*help ivreg
ivreg l_Q (l_P=cartel), robust first

ivreg l_Q seas* (l_P=cartel), robust first

* see ivreg2 tests
ivreg2 l_Q seas* (l_P=cartel), robust first


// (e)
test l_P = -1 

* e in (-1.1 , 0.8)

// (f)
ivreg l_Q (l_P=ice) , robust


* Using esttab to plot all

label var l_P "Ln Price"
label var l_P "Ln Quant."

eststo clear
qui: eststo: reg l_Q l_P , robust
qui: eststo: reg l_Q l_P seas*, robust
qui: eststo: ivreg l_Q (l_P=cartel) seas* , robust
qui: eststo: ivreg l_Q (l_P=ice) , robust

esttab, se r2 starlevels(* 0.10 ** 0.05 *** 0.01) keep(l_P) label






* Ex 6

//Sets option more off
set more off

//Sets working directory
cd "C:\Users\gidip\Dropbox\2020 - Econometrics\Psets\Pset3\eminent_domain"

//Clears previously loaded dataset 
clear all

//Imports dataset
import delimited "temp_CSHomePrice.mat.csv"

//Reordering variables
order year circuit 

//Collapses data, taking the within year-circuit mean
collapse (mean) logpriceindex-numpanels3x_protestant, by(year circuit)

sort circuit year

//Running required regression
reg logpriceindex numpro_casecat_12

//Storing dataset, replacing file if it already exists
save CSprice.dta, replace

//clears previously loaded dataset
clear

//loading probability
import delimited probs.txt

//Renaming matching variables (they must have the same name as in CSprice)
rename scircuit circuit
rename syear year

//Merging datasets
merge 1:1 circuit year using CSprice

//Creating local variable with controls
local controls = "missing_cy_12 numcasecat_12 prob_1x_dem prob_2x_dem prob_3x_dem prob_1x_female prob_2x_female prob_3x_female prob_1x_nonwhite prob_2x_nonwhite prob_3x_nonwhite prob_1x_black prob_2x_black prob_3x_black prob_1x_jewish prob_2x_jewish prob_3x_jewish prob_1x_catholic prob_2x_catholic prob_3x_catholic prob_1x_noreligion prob_2x_noreligion prob_3x_noreligion prob_1x_instate_ba prob_2x_instate_ba prob_3x_instate_ba prob_1x_ba_public prob_2x_ba_public prob_3x_ba_public prob_1x_jd_public prob_2x_jd_public prob_3x_jd_public prob_1x_elev prob_2x_elev prob_3x_elev prob_1x_female_black prob_2x_female_black prob_3x_female_black prob_1x_female_noreligion prob_2x_female_noreligion prob_3x_female_noreligion prob_1x_black_noreligion prob_2x_black_noreligion prob_3x_black_noreligion prob_1x_female_jd_public prob_2x_female_jd_public prob_3x_female_jd_public prob_1x_black_jd_public prob_2x_black_jd_public prob_3x_black_jd_public prob_1x_noreligion_jd_public prob_2x_noreligion_jd_public prob_3x_noreligion_jd_public prob_1x_mainline prob_2x_mainline prob_3x_mainline prob_1x_evangelical prob_2x_evangelical prob_3x_evangelical prob_1x_protestant prob_2x_protestant prob_3x_protestant"
*ssc install ivreg2

//2SLS
ivreg2 logpriceindex `controls' (numpro_casecat_12 = numpanels1x_dem), first


local controls = "missing_cy_12 numcasecat_12 prob_1x_dem prob_2x_dem prob_3x_dem prob_1x_female prob_2x_female prob_3x_female prob_1x_nonwhite prob_2x_nonwhite prob_3x_nonwhite prob_1x_black prob_2x_black prob_3x_black prob_1x_jewish prob_2x_jewish prob_3x_jewish prob_1x_catholic prob_2x_catholic prob_3x_catholic prob_1x_noreligion prob_2x_noreligion prob_3x_noreligion prob_1x_instate_ba prob_2x_instate_ba prob_3x_instate_ba prob_1x_ba_public prob_2x_ba_public prob_3x_ba_public prob_1x_jd_public prob_2x_jd_public prob_3x_jd_public prob_1x_elev prob_2x_elev prob_3x_elev prob_1x_female_black prob_2x_female_black prob_3x_female_black prob_1x_female_noreligion prob_2x_female_noreligion prob_3x_female_noreligion prob_1x_black_noreligion prob_2x_black_noreligion prob_3x_black_noreligion prob_1x_female_jd_public prob_2x_female_jd_public prob_3x_female_jd_public prob_1x_black_jd_public prob_2x_black_jd_public prob_3x_black_jd_public prob_1x_noreligion_jd_public prob_2x_noreligion_jd_public prob_3x_noreligion_jd_public prob_1x_mainline prob_2x_mainline prob_3x_mainline prob_1x_evangelical prob_2x_evangelical prob_3x_evangelical prob_1x_protestant prob_2x_protestant prob_3x_protestant"
reg logpriceindex numpro_casecat_12 `controls'

// The instrument is weak (irrelevant)!

// Why?> See the F from the 1st stage = Thumb rule this guy shoulb be larger than 12 (or 20) -> Instrument is weak.
