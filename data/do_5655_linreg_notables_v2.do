capture log close
log using statalog_5655_linreg_notables_v2.log, replace
import delimited "analysis_sample_5655_v2.csv"

* estout: http://repec.org/bocode/e/estout/hlp_estout.html

*******************************************
**** ADDITIONAL SAMPLE RESTRICTIONS *******
*******************************************
keep if num_waves == 5
*******************************************

xtdes, i(g1_id) t(wave)

tabulate wave, generate(d)
tabulate g1_state, generate(s)

* Convert string to numeric
gen tmp = fmhpi
drop fmhpi
gen fmhpi = real(tmp)
drop tmp
gen tmp = state_ur
drop state_ur
gen state_ur = real(tmp)
drop tmp


*******************************************************
**** Create Variables and Macros **********************
*******************************************************

gen g0_transfer_inc1000 = g0_transfer_inc/1000
gen g1_child_exp1000 = g1_child_exp/1000

global allvars g0_m_married g0_m_employed g0_max_age g1_kid1_fu g1_kid2_fu g1_child_exp1000 g1_emphealth g1_pubhealth g1_job_search g1_some_coll g0_emphealth g0_medicare g0_own g0_pension g0_d_retired g0_transfer_inc1000 state_ur

* xbars (time averages)
sort g1_id wave
global vars1bar
foreach v in $allvars{
by g1_id: egen `v'bar = mean(`v')
}

global allvarsbar g0_m_marriedbar g0_m_employedbar g1_kid1_fubar g1_kid2_fubar g1_child_exp1000bar g1_emphealthbar g1_pubhealthbar g1_job_searchbar g1_some_collbar g0_emphealthbar g0_medicarebar g0_ownbar g0_pensionbar g0_max_agebar g0_d_retiredbar g0_transfer_inc1000bar state_urbar

global vars1_pols g0_m_married g0_medicare g1_kid1_fu g0_own g1_emphealth state_ur g0_transfer_inc1000 g1_job_search 

global vars1 g0_m_married g0_medicare g1_kid1_fu g0_own g1_emphealth state_ur g0_transfer_inc1000 g1_job_search d2 d3 d4 d5

global vars1bar g0_m_marriedbar g0_medicarebar g1_kid1_fubar g0_ownbar g1_emphealthbar state_urbar g0_transfer_inc1000bar g1_job_searchbar

***************************************************************************
**** Linear Regression Models (POLS, RE, FE, HT) **************************
***************************************************************************

*** POLS v RE ***

reg cores $vars1_pols
est store pols1
xtreg cores $vars1, re
test d2 d3 d4 d5
est store re1 


* H0: var(c_i) = 0 (POLS is preferred over RE)
sort g1_id wave
predict vhat, ue
by g1_id: gen vhat_1 = vhat[_n-1]
reg vhat vhat_1

*** FE v. RE ***

xtreg cores $vars1, fe
test $vars1
test d2 d3 d4 d5
est store fe1

*H0:  FE not asymptotically different than RE, i.e., RE is consistent (thus preferred over FE) since X is not correlated with c (RE exogeneity assumption holds).

 * Hausman assumes homoskedastic errors. Should also do the Mundlak (1978) Hausman, i.e., including the time averages of the regressors on the RHS of a RE regression equation using heteroskedasticity-robust standard errors.
 
 * H0: Time averages are jointly insignificant (RE preferred to FE).

* Mundlak (1978) Hausman
xtreg cores $vars1 $vars1bar, re vce(robust)

test $vars1bar

* Reject H0; FE preferred over RE. (Implications on Chamberlain RE Probit?)

* FE with HAC standard errors.
xtreg cores $vars1, fe vce(cluster g1_id)
test $vars1
test d2 d3 d4 d5
est store fe1hac


*** Output and predict 

eststo m1: quietly reg cores $vars1_pols
predict pols1hat, xb
egen pols1hat_mean = mean(pols1hat)
gen pols1hat_bern = (pols1hat >= pols1hat_mean)
table cores pols1hat_bern

eststo m2: quietly xtreg cores $vars1, re
predict re1hat, xb
egen re1hat_mean = mean(re1hat)
gen re1hat_bern = (re1hat >= re1hat_mean)
table cores re1hat_bern

eststo m3: quietly xtreg cores $vars1, fe
predict fe1hat, xb
egen fe1hat_mean = mean(fe1hat)
gen fe1hat_bern = (fe1hat >= fe1hat_mean)
table cores fe1hat_bern

eststo m4: quietly xtreg cores $vars1, fe cluster(g1_id)
predict fe1hachat, xb
egen fe1hachat_mean = mean(fe1hachat)
gen fe1hachat_bern = (fe1hachat >= fe1hachat_mean)
table cores fe1hachat_bern

esttab m1 m2 m3 m4 using results_linreg_betas.csv, varwidth(30) se replace


log close
