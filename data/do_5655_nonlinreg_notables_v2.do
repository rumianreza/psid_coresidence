capture log close
log using statalog_5655_nonlinreg_notables_v2.log, replace
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

global allvarsbar g0_m_marriedbar g0_m_employedbar g0_max_agebar g1_kid1_fubar g1_kid2_fubar g1_child_exp1000bar g1_emphealthbar g1_pubhealthbar g1_job_searchbar g1_some_collbar g0_emphealthbar g0_medicarebar g0_ownbar g0_pensionbar g0_d_retiredbar g0_transfer_inc1000bar state_urbar

global vars1 g0_m_married g0_medicare g1_kid1_fu g0_own g1_emphealth state_ur g0_transfer_inc1000 g1_job_search d2 d3 d4 d5

global vars1bar g0_m_marriedbar g0_medicarebar g1_kid1_fubar g0_ownbar g1_emphealthbar state_urbar g0_transfer_inc1000bar g1_job_searchbar

***************************************************************************
**** Nonlinear Regression Models (pooled probit, RE probit, Chamberlain
**** Probit, FE Logit) ****************************************************
***************************************************************************

*** pooled probit vs. RE Probit vs. Chamberlain Probit ***
** We compare these 3 models by comparing % correctly predicted (see Section 8)


*** pooled probit ***
probit cores $vars1, nolog
test d2 d3 d4 d5
est store pprob1
predict prob1hat, pr
egen prob1hat_mean = mean(prob1hat)
gen prob1hat_bern = (prob1hat >= prob1hat_mean)
tab cores prob1hat_bern
margins, dydx(*) atmeans post
est store PEA_prob1
quietly probit cores $vars1, nolog
margins, dydx(*) post
est store APE_prob1
est table PEA_prob1 APE_prob1

*** HAC pooled probit ***
probit cores $vars1, vce(cluster g1_id) nolog
test d2 d3 d4 d5
est store pprob1hac
margins, dydx(*) atmeans post
est store PEA_prob1hac
quietly probit cores $vars1, vce(cluster g1_id) nolog
margins, dydx(*) post
est store APE_prob1hac
est table PEA_prob1hac APE_prob1hac

est table pprob1 pprob1hac

*** RE probit ***
xtprobit cores $vars1, re nolog
test d2 d3 d4 d5
est store pprob1re
predict reprob1hat, pu0
egen reprob1hat_mean = mean(reprob1hat)
gen reprob1hat_bern = (reprob1hat >= reprob1hat_mean)
tab cores reprob1hat_bern
margins, dydx(*) atmeans post
est store PEA_prob1re
quietly xtprobit cores $vars1, re nolog
margins, dydx(*) post
est store APE_prob1re
est table PEA_prob1re APE_prob1re

*** HAC RE probit ***
xtprobit cores $vars1, re vce(cluster g1_id) nolog
test d2 d3 d4 d5
est store pprob1rehac
margins, dydx(*) atmeans post
est store PEA_prob1rehac
quietly xtprobit cores $vars1, re vce(cluster g1_id) nolog
margins, dydx(*) post
est store APE_prob1rehac
est table PEA_prob1rehac APE_prob1rehac

est table pprob1re pprob1rehac

*** Chamberlain Probit ***
xtprobit cores $vars1 $vars1bar, re nolog
test d2 d3 d4 d5
est store chamb1
** pu0 is available for RE probit (i.e., sets c_i equal to 0 before predicting)
predict chamb1hat, pu0
egen chamb1hat_mean = mean(chamb1hat)
gen chamb1hat_bern = (chamb1hat >= chamb1hat_mean)
tab cores chamb1hat_bern
quietly xtprobit cores $vars1 $vars1bar, re nolog
margins, dydx(*) atmeans post
est store PEA_chamb1
quietly xtprobit cores $vars1 $vars1bar, re nolog
margins, dydx(*) post
est store APE_chamb1
est table PEA_chamb1 APE_chamb1

* H0: Time averaged regressors are insignificant (RE preferred to Chamberlain)
test $vars1bar

*** HAC Chamberlain Probit
xtprobit cores $vars1 $vars1bar, re vce(cluster g1_id) nolog
test d2 d3 d4 d5
est store chamb1hac
** pu0 is available for RE probit (i.e., sets c_i equal to 0 before predicting)
predict chamb1hachat, pu0
gen chamb1hachat_bern = (chamb1hachat >= .1)
tab cores chamb1hachat_bern
quietly xtprobit cores $vars1 $vars1bar, re vce(cluster g1_id) nolog
margins, dydx(*) atmeans post
est store PEA_chamb1hac
quietly xtprobit cores $vars1 $vars1bar, re vce(cluster g1_id) nolog
margins, dydx(*) post
est store APE_chamb1hac
est table PEA_chamb1hac APE_chamb1hac

* H0: Time averaged regressors are insignificant (RE preferred to Chamberlain)
test $vars1bar

* Reject H0: chamberlain RE probit is necessary.

*** esttab ***
* http://repec.org/bocode/e/estout/esttab.html
* http://repec.sowi.unibe.ch/stata/estout/esttab.html
eststo est1: quietly probit cores $vars1
eststo m1: margins, dydx(*) atmeans post
eststo est2: quietly probit cores $vars1, vce(cluster g1_id)
eststo m2: margins, dydx(*) atmeans post
eststo est3: quietly xtprobit cores $vars1
eststo m3: margins, dydx(*) atmeans post
eststo est4: quietly xtprobit cores $vars1, vce(cluster g1_id)
eststo m4: margins, dydx(*) atmeans post
eststo est5: quietly xtprobit cores $vars1 $vars1bar
eststo m5: margins, dydx(*) atmeans post
eststo est6: quietly xtprobit cores $vars1 $vars1bar, vce(cluster g1_id)
eststo m6: margins, dydx(*) atmeans post

esttab est1 est2 est3 est4 est5 est6 using results_nonlinreg.csv, varwidth(30) se replace

esttab m1 m2 m3 m4 m5 m6 using results_nonlinreg_margins.csv, varwidth(30) se replace


log close
