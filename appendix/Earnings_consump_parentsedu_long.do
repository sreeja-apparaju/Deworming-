
 * Filename: Earnings_consump_parendsedu_long.do
 * Table: Appendix Table S8
 * Description: This do file creates the table which shows the effect of 
 *		parents' average level of education on the main outcomes (consumption,
 *		individual earnings, and household earnings). 

********************************************************************************

clear all
set more off

//Use pooled data and merge with parents education
use pupid parents_education using "$data/Worms20_Heterogeneity.dta",clear
drop if parents_education==.
duplicates drop
tempfile parents_edu
save `parents_edu'

use "$data/Worms20_Analysis.dta",clear
merge m:1 pupid using `parents_edu',keep(3) nogen

//Demean parents education variable
summ parents_education [aw=weight],d
local avg_parents_education = `r(mean)'
replace parents_education = parents_education - `avg_parents_education'

//Interact parents education with treatment, cost-sharing, and saturation
gen treatXparents_edu = treatment * parents_education
gen costXparents_edu = cost_sharing * parents_education
gen satXparents_edu = saturation_dm * parents_education

//Create 3 sets of outcomes so table can be "long" rather than tall
foreach var of varlist tot_cnsp_t tot_earn12m_t tot_hhearn12m_t {
	gen `var'_older = `var'
	gen `var'_male = `var'
}

tempfile all_data
save `all_data'

preserve
	keep if older==1
	tempfile older_data
	save `older_data'
restore

preserve
	keep if male==1
	tempfile male_data
	save `male_data'
restore

//Define local outcomes
local outcomes tot_cnsp_t tot_cnsp_t_male tot_cnsp_t_older tot_earn12m_t tot_earn12m_t_male tot_earn12m_t_older tot_hhearn12m_t tot_hhearn12m_t_male tot_hhearn12m_t_older
		
//Initialize latex document
texdoc init "$table/KLPS4_E+_pooled_earnings_consumption_parentsedu_long.tex", replace force
texdoc write \\ \toprule
texdoc close

//Save regression estimates
foreach outcome of local outcomes {

	if "`outcome'" == "tot_cnsp_t" | "`outcome'" == "tot_earn12m_t" {
		use `all_data',clear
		
		sum `outcome' if treatment==0 [aw=weight]
		local controlMean = r(mean)
		
		eststo: reg `outcome' treatment treatXparents_edu costXparents_edu satXparents_edu parents_education $x_controls_panel [pw=weight], cluster(psdpsch98)
	}
	
	*Household earnings uses cross-sectional controls instead of panel controls
	else if "`outcome'" == "tot_hhearn12m_t" {
		use `all_data',clear
		
		sum `outcome' if treatment==0 [aw=weight]
		local controlMean = r(mean)
		
		eststo: reg `outcome' treatment treatXparents_edu costXparents_edu satXparents_edu parents_education $x_controls1 [pw=weight], cluster(psdpsch98)
	}	
	
	else if "`outcome'" == "tot_cnsp_t_older" | "`outcome'" == "tot_earn12m_t_older" {
		use `older_data',clear
		
		sum `outcome' if treatment==0 [aw=weight]
		local controlMean = r(mean)
		
		eststo: reg `outcome' treatment treatXparents_edu costXparents_edu satXparents_edu parents_education  $x_controls_panel [pw=weight], cluster(psdpsch98)
	}
	
	*Household earnings uses cross-sectional controls instead of panel controls
	else if "`outcome'" == "tot_hhearn12m_t_older" {
		use `older_data',clear
		
		sum `outcome' if treatment==0 [aw=weight]
		local controlMean = r(mean)
		
		eststo: reg `outcome' treatment treatXparents_edu costXparents_edu satXparents_edu parents_education  $x_controls1 [pw=weight], cluster(psdpsch98)
	}	
	
	else if "`outcome'" == "tot_cnsp_t_male" | "`outcome'" == "tot_earn12m_t_male" {
		use `male_data', clear
		
		sum `outcome' if treatment==0 [aw=weight]
		local controlMean = r(mean)
		
		eststo: reg `outcome' treatment treatXparents_edu costXparents_edu satXparents_edu parents_education  $x_controls_panel [pw=weight], cluster(psdpsch98)
	}
	
	*Household earnings uses cross-sectional controls instead of panel controls
	else if "`outcome'" == "tot_hhearn12m_t_male" {
		use `male_data',clear
		
		sum `outcome' if treatment==0 [aw=weight]
		local controlMean = r(mean)
		
		eststo: reg `outcome' treatment treatXparents_edu costXparents_edu satXparents_edu parents_education $x_controls1 [pw=weight], cluster(psdpsch98)
	}	

	//Add scalars in regression table
		*Control mean
		estadd scalar control_mean = round(`controlMean',1)
		local treat = _b[treatment]
		local teffect = 100*(`treat'/`controlMean')
	
		*Treatment effect
		estadd scalar t_effect = round(`teffect', .01)

		*P-value on treatment
		local t = _b[treatment]/_se[treatment]
		local pvalue = 2*ttail(e(df_r),abs(`t'))
		estadd scalar pvalue = `pvalue'
		
		*Num observations
		count if e(sample)
		estadd scalar N_ind=r(N)
		
		*F-test
		test treatXparents_edu costXparents_edu satXparents_edu
		local ftest_pval : display %9.3f `r(p)'
		estadd scalar ftest `ftest_pval'
}

//Label variables
label variable treatment "Treatment ($\lambda_1$)"
label variable treatXparents_edu "Treatment x Parents' Average Education"
label variable cost_sharing "Cost Sharing ($\lambda_2$)"
label variable costXparents_edu "Cost Sharing x Parents' Average Education"
label variable saturation_dm "Saturation ($\lambda_3$)"
label variable satXparents_edu "Saturation x Parents' Average Education"
label variable parents_education "Parents' Average Education"

//Export Table
# delimit ;
	esttab using "$table/KLPS4_E+_pooled_earnings_consumption_parentsedu_long.tex",  append
		cells ("b(star fmt(%12.0f))"
				"se(par fmt(%12.0f))")
		nocons nolz nolines star(* .10 ** .05 *** .01)
		label se noobs collabels(none) fragment nomtitles booktabs nonumbers posthead(\hline)
		mgroups("\bettershortstack{Annual Per-Capita\\Consumption}" "\bettershortstack{Annual Individual\\Earnings}" "\bettershortstack{Annual Per-Capita\\Household Earnings}", pattern(1 0 0 1 0 0 1 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
		mlabels("\multicolumn{1}{c}{\bettershortstack{(1)\\Full\\Sample}}" "\multicolumn{1}{c}{\bettershortstack{(2)\\Male\\Subsample}}" "\multicolumn{1}{c}{\bettershortstack{(3)\\Older\\Subsample}}" "\multicolumn{1}{c}{\bettershortstack{(4)\\Full\\Sample}}" "\multicolumn{1}{c}{\bettershortstack{(5)\\Male\\Subsample}}" "\multicolumn{1}{c}{\bettershortstack{(6)\\Older\\Subsample}}" "\multicolumn{1}{c}{\bettershortstack{(7)\\Full\\Sample}}" "\multicolumn{1}{c}{\bettershortstack{(8)\\Male\\Subsample}}" "\multicolumn{1}{c}{\bettershortstack{(9)\\Older\\Subsample}}")
		keep(treatment cost_sharing saturation_dm treatXparents_edu costXparents_edu satXparents_edu parents_education)
		order(treatment cost_sharing saturation_dm treatXparents_edu costXparents_edu satXparents_edu parents_education)		
		stats(control_mean t_effect ftest N_ind ,
			labels("\hline Control Mean" "Treatment Effect (\%)" "Joint F-Test (p-value)" "Number Observations") fmt(%12.0g %12.1f %12.3f %12.0f))
			substitute("Standard errors in parentheses" " "
					"\sym{*} \(p<.10\), \sym{**} \(p<.05\), \sym{***} \(p<.01\)" " " \_ _)
;
#delimit cr

estimates drop _all

