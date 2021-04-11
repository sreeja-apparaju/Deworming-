
clear all
set more off

************************
* PANEL A: FULL SAMPLE *
************************

//Use pooled data
use "$data/Worms20_Analysis.dta",clear

local outcomes tot_cnsp_t tot_earn12m_t tot_hhearn12m_t

local colnames "\bettershortstack{(1)\\ \\Per Capita\\Consumption} & \bettershortstack{(2)\\ \\Total\\Earnings} & \bettershortstack{(3)\\Per Capita\\Household\\Earnings} "
	
local panel_name_all "Panel A: Full Sample"
local panel_name_female "Panel B: Female"
local panel_name_male "Panel C: Male"
	
*get number of columns
local num_columns : word count `outcomes' + 5

** initialize tex document
texdoc init "$table/KLPS4_E+_pooled_outcomes_extended.tex", replace force
texdoc write \\ \toprule
texdoc close

*Save regression estimates
foreach outcome of local outcomes {

	sum `outcome' if treatment==0 [aw=weight]
	local controlMean = r(mean)
	
	if "`outcome'" == "tot_hhearn12m_t" {
		eststo: reg `outcome' treatment $x_controls1 [pw=weight], cluster(psdpsch98)
	}
	else {
		eststo: reg `outcome' treatment $x_controls_panel [pw=weight], cluster(psdpsch98)
	}
	
	*add scalars in regression table
	estadd scalar control_mean = round(`controlMean',1)
	local treat = _b[treatment]
	local teffect = 100*(`treat'/`controlMean')
	
	*treatment effect
	estadd scalar t_effect = round(`teffect', .01)

	*p-value on treatment
	local t = _b[treatment]/_se[treatment]
	local pvalue = 2*ttail(e(df_r),abs(`t'))
	estadd scalar pvalue = `pvalue'
	
	*num observations
	count if e(sample)
	estadd scalar N_ind=r(N)
	
	*f-test
	test treatment cost_sharing saturation_dm
	local ftest_pval : display %9.3f `r(p)'
	estadd scalar ftest `ftest_pval'
	
}

*label variables
label variable treatment "Treatment ($\lambda_1$)"
label variable cost_sharing "Cost Sharing ($\lambda_2$)"
label variable saturation_dm "Saturation ($\lambda_3$)"


*Exporting Panel A
# delimit ;
	esttab using "$table/KLPS4_E+_pooled_outcomes_extended.tex",  append
		cells ("b(star fmt(%12.0f))"
				"se(par fmt(%12.0f))")
		nocons nolz nolines star(* .10 ** .05 *** .01)
		label se noobs collabels(none) fragment nomtitles booktabs nonumbers posthead(\hline)
		mgroups("`colnames'", span) 
		keep(treatment cost_sharing saturation_dm)
		stats(control_mean t_effect ftest N_ind ,
			labels("\hline Control Mean" "Treatment Effect (\%)" "Joint F-Test (p-value)" "Number Observations") fmt(%12.0g %12.1f %12.3f %12.0f))
			substitute("Standard errors in parentheses" " "
					"\sym{*} \(p<.10\), \sym{**} \(p<.05\), \sym{***} \(p<.01\)" " " \_ _)
;
#delimit cr

estimates drop _all

*******************
* PANEL B: EARNINGS *
*******************

texdoc init "$table/KLPS4_E+_pooled_outcomes_extended.tex", append force
texdoc write \midrule
texdoc close

//Use pooled data
use "$data/Worms20_Analysis.dta",clear

local outcomes tot_ln_earn12m_t wage_earn12m_t self_earn12m_t farm_earn12m_t nonzero_earn_12m tot_hrearn12m_t tot_wealth_t

local colnames "\bettershortstack{(1)\\ \\Log Yearly\\Earnings} & \bettershortstack{(2)\\ \\Wage\\Earnings} & \bettershortstack{(3)\\Self-\\Employment\\Earnings} & \bettershortstack{(4)\\ \\Farming\\Profit} & \bettershortstack{(5)\\ \\Non-Zero\\Earnings} & \bettershortstack{(6)\\ \\Hourly\\Earnings} & \bettershortstack{(7)\\Per Capita\\Household\\Wealth} & "
	
*Save regression estimates
foreach outcome of local outcomes {

	sum `outcome' if treatment==0 [aw=weight]
	local controlMean = r(mean)
	
	eststo: reg `outcome' treatment $x_controls_panel [pw=weight], cluster(psdpsch98)

	if "`outcome'" == "tot_ln_earn12m_t"  {			
		estadd scalar control_mean = round(`controlMean',.01)
		local treat = _b[treatment]
		local teffect = 100 * log(1 + `treat')
	}
	
	else if "`outcome'"=="tot_hrearn12m_t" | "`outcome'" == "nonzero_earn_12m" {
		estadd scalar control_mean = round(`controlMean',.01)
		local treat = _b[treatment]
		local teffect = 100*(`treat'/`controlMean')
	}
	
	else {
		*add scalars in regression table
		estadd scalar control_mean = round(`controlMean',1)
		local treat = _b[treatment]
		local teffect = 100*(`treat'/`controlMean')
	}
	
	*treatment effect
	estadd scalar t_effect = round(`teffect', .01)

	*p-value on treatment
	local t = _b[treatment]/_se[treatment]
	local pvalue = 2*ttail(e(df_r),abs(`t'))
	estadd scalar pvalue = `pvalue'
	
	*num observations
	count if e(sample)
	estadd scalar N_ind=r(N)
	
	*f-test
	test treatment cost_sharing saturation_dm
	local ftest_pval : display %9.3f `r(p)'
	estadd scalar ftest `ftest_pval'

}

*label variables
label variable treatment "Treatment ($\lambda_1$)"
label variable cost_sharing "Cost Sharing ($\lambda_2$)"
label variable saturation_dm "Saturation ($\lambda_3$)"


*Exporting Panel B
# delimit ;
	esttab using "$table/KLPS4_E+_pooled_outcomes_extended.tex",  append
		cells ("b(star fmt(%12.0f) pattern(0 1 1 1 0 0 1)) b(star fmt(%12.2f) pattern(1 0 0 0 1 1 0))"
				"se(par fmt(%12.0f) pattern(0 1 1 1 0 0 1)) se(par fmt(%12.2f) pattern(1 0 0 0 1 1 0))")
		nocons nolz nolines star(* .10 ** .05 *** .01)
		label se noobs collabels(none) fragment nomtitles booktabs nonumbers posthead(\hline)
		mgroups("`colnames'", span) 
		keep(treatment cost_sharing saturation_dm)
		stats(control_mean t_effect ftest N_ind ,
			labels("\hline Control Mean" "Treatment Effect (\%)" "Joint F-Test (p-value)" "Number Observations") fmt(%12.0g %12.1f %12.3f %12.0f))
			substitute("Standard errors in parentheses" " "
					"\sym{*} \(p<.10\), \sym{**} \(p<.05\), \sym{***} \(p<.01\)" " " \_ _)
;
#delimit cr

estimates drop _all



**********************************
* PANEL C: LABOR OCCCHOICE WEALTH*
**********************************

texdoc init "$table/KLPS4_E+_pooled_outcomes_extended.tex", append force
texdoc write \midrule
texdoc close

//Use pooled data
use "$data/Worms20_Analysis.dta",clear

local outcomes urban tot_hrs farm_hrs nonfarm_hrs emp_ag_fish emp_services_wsale emp_construct_contract emp_manufacturing

local colnames "\bettershortstack{(1)\\ \\ \\Urban\\Residence} & \bettershortstack{(2)\\ \\ \\Total\\Hours Worked} & \bettershortstack{(3)\\ \\Hours\\Worked - \\Agriculture} & \bettershortstack{(4)\\ \\Hours\\Worked -\\Non-Agriculture} & \bettershortstack{(5)\\ \\Employed -\\Agriculture/ \\Fishing} & \bettershortstack{(6)\\Employed -\\Services/ \\Wholesale/ \\Retail} & \bettershortstack{(7)\\Employed -\\Construction/ \\Trade\\Contractor} & \bettershortstack{(8)\\ \\ \\Employed -\\Manufacturing}"

*get number of columns
local num_columns : word count `outcomes'

*Save regression estimates
foreach outcome of local outcomes {

	sum `outcome' if treatment==0 [aw=weight]
	local controlMean = r(mean)
	
	eststo: reg `outcome' treatment $x_controls_panel [pw=weight], cluster(psdpsch98)

	*add scalars in regression table
	if  "`outcome'" == "urban" | "`outcome'" == "tot_hrs" | "`outcome'" =="farm_hrs" | "`outcome'" == "nonfarm_hrs" {
		estadd scalar control_mean = round(`controlMean',.01)
		local treat = _b[treatment]
		local teffect = 100*(`treat'/`controlMean')
	}	
	else {
		estadd scalar control_mean = round(`controlMean',.001)
		local treat = _b[treatment]
		local teffect = 100*(`treat'/`controlMean')
	}
	
	*treatment effect
	estadd scalar t_effect = round(`teffect', .01)

	*p-value on treatment
	local t = _b[treatment]/_se[treatment]
	local pvalue = 2*ttail(e(df_r),abs(`t'))
	estadd scalar pvalue = `pvalue'
	
	*num observations
	count if e(sample)
	estadd scalar N_ind=r(N)
	
	*f-test
	test treatment cost_sharing saturation_dm
	local ftest_pval : display %9.3f `r(p)'
	estadd scalar ftest `ftest_pval'

}

*label variables
label variable treatment "Treatment ($\lambda_1$)"
label variable cost_sharing "Cost Sharing ($\lambda_2$)"
label variable saturation_dm "Saturation ($\lambda_3$)"


*Exporting Panel C
# delimit ;
	esttab using "$table/KLPS4_E+_pooled_outcomes_extended.tex",  append
		cells ("b(star fmt(%12.2f) pattern(1 1 1 1 0 0 0 0)) b(star fmt(%12.3f) pattern(0 0 0 0 1 1 1 1))"
				"se(par fmt(%12.2f) pattern(1 1 1 1 0 0 0 0)) se(par fmt(%12.3f) pattern(0 0 0 0 1 1 1 1))")
		nocons nolz nolines star(* .10 ** .05 *** .01)
		label se noobs collabels(none) fragment nomtitles booktabs nonumbers posthead(\hline) postfoot("\bottomrule")
		mgroups("`colnames'", span) 
		keep(treatment cost_sharing saturation_dm)
		stats(control_mean t_effect ftest N_ind ,
			labels("\hline Control Mean" "Treatment Effect (\%)" "Joint F-Test (p-value)" "Number Observations") fmt(%12.0g %12.1f %12.3f %12.0f))
			substitute("Standard errors in parentheses" " "
					"\sym{*} \(p<.10\), \sym{**} \(p<.05\), \sym{***} \(p<.01\)" " " \_ _)
;
#delimit cr

estimates drop _all
