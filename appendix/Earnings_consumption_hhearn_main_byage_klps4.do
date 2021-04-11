 
 * Filename: Earnings_consumption_hhearn_main_byage_klps4.do
 * Table: Appendix Table S3
 * Description: This do file creates the table which shows the treatment effect 
 *		on consumption, earnings, and household earnings using KLPS-4
 *		cross-sectional data.

********************************************************************************

clear all
set maxvar 10000
set more off
	
//Initialize Latex document
	texdoc init "$output/KLPS4_E+_earnings_consumption_main_byage_klps4.tex", replace force
	texdoc write \\ \toprule
	texdoc close
	
****************
*FDR ADJUSTMENT*
****************
//Save p-values in order to run FDR adjustment to get adjusted q-values	
	
gen outcome=""
gen pval=.

tempfile pval_all pval_gender pval_age
save `pval_all'
save `pval_gender'
save `pval_age'

//Use KLPS-4 data
use "$data/Worms20_Analysis.dta",clear
keep if interview_round==4

//Create interaction terms
foreach var of varlist female male older younger {
	gen treatX`var' = treatment * `var'
	gen costX`var' = cost_sharing * `var'
	gen satX`var' = saturation_dm * `var'
}

tempfile all_data
save `all_data'

//Define outcomes (consumption, earnings)
local outcomes_FDR tot_cnsp_t tot_earn12m_t

foreach outcome of local outcomes_FDR {
	//Full sample
		use `all_data',clear
		eststo: reg `outcome' treatment $x_controls1 [pw=weight], cluster(psdpsch98)
		
		local t = _b[treatment]/_se[treatment]
		local pvalue = 2*ttail(e(df_r),abs(`t'))
			
		*Saving all p-values in dataset for FDR adjustment
		preserve
			gen outcome = "`outcome'"
			gen pval = `pvalue'
			
			keep outcome pval
			duplicates drop
			
			append using `pval_all'
			save `pval_all',replace
		restore
	
	//Females
		use `all_data',clear
		eststo: reg `outcome' treatment treatXmale costXmale satXmale cost_sharing saturation_dm  avgtest96 pup_pop demeaned_popT_6k zoneidI2-zoneidI8  male std98_base_I2-std98_base_I6 month_interviewI2-month_interviewI12 wave2 voced [pw=weight], cluster(psdpsch98)

		local t = _b[treatment]/_se[treatment]
		local pvalue = 2*ttail(e(df_r),abs(`t'))

		*Saving all p-values in dataset for FDR adjustment
		preserve
			gen outcome = "`outcome'"
			gen pval_fem=1
			gen pval = `pvalue'
			
			keep outcome pval pval_fem
			duplicates drop
			
			append using `pval_gender'
			save `pval_gender',replace
		restore
		
	//Males 
		use `all_data',clear
		eststo: reg `outcome' treatment treatXfemale costXfemale satXfemale $x_controls1 [pw=weight], cluster(psdpsch98)
		
		local t = _b[treatment]/_se[treatment]
		local pvalue = 2*ttail(e(df_r),abs(`t'))
			
		*Saving all p-values in dataset for FDR adjustment
		preserve
			gen outcome = "`outcome'"
			gen pval_fem=0
			gen pval = `pvalue'
			
			keep outcome pval pval_fem
			duplicates drop
			
			append using `pval_gender'
			save `pval_gender',replace
		restore
		
	//Older
		use `all_data',clear
		drop if older==.
		eststo: reg `outcome' treatment treatXyounger costXyounger satXyounger younger $x_controls1 [pw=weight], cluster(psdpsch98)
		
		local t = _b[treatment]/_se[treatment]
		local pvalue = 2*ttail(e(df_r),abs(`t'))
			
		*Saving all p-values in dataset for FDR adjustment
		preserve
			gen outcome = "`outcome'"
			gen pval = `pvalue'
			gen pval_old=1
			
			keep outcome pval pval_old
			duplicates drop
			
			append using `pval_age'
			save `pval_age',replace
		restore

	//Younger
		use `all_data',clear
		drop if older==.
		eststo: reg `outcome' treatment treatXolder costXolder satXolder older $x_controls1 [pw=weight], cluster(psdpsch98)
		
		local t = _b[treatment]/_se[treatment]
		local pvalue = 2*ttail(e(df_r),abs(`t'))
			
		*Saving all p-values in dataset for FDR adjustment
		preserve
			gen outcome = "`outcome'"
			gen pval = `pvalue'
			gen pval_old=0
			
			keep outcome pval pval_old
			duplicates drop
			
			append using `pval_age'
			save `pval_age',replace
		restore
}

//Run FDR adjustment
preserve
	//Full sample
		fdr_adjustment "`pval_all'"
		keep outcome qval_fdr

		*Save in tempfile to merge into regression table	
		tempfile qval_data
		save `qval_data'
	
	//Female & male
		fdr_adjustment "`pval_gender'"
		keep outcome qval_fdr pval_fem
		tempfile qval_gender
		save `qval_gender'
				
	//Older & younger
		fdr_adjustment "`pval_age'"
		keep outcome qval_fdr pval_old
		tempfile qval_age
		save `qval_age'
restore

estimates drop _all		

**********************
*PANEL A: CONSUMPTION*	
**********************

//Use KLPS-4 data
use "$data/Worms20_Analysis.dta",clear 
keep if interview_round==4

//Create interaction variables
gen treatXfemale = treatment * female
gen costXfemale = cost_sharing * female
gen satXfemale = saturation_dm * female

gen treatXolder = treatment * older
gen costXolder = cost_sharing * older
gen satXolder = saturation_dm * older

//Run regressions	
foreach outcome in tot_cnsp_t{

	*COL 1/5: Full sample
		sum `outcome' if treatment==0 [aw=weight]
		local controlMean = r(mean)

		eststo: reg `outcome' treatment $x_controls1 [pw=weight], cluster(psdpsch98)

		*Add scalars
		estadd scalar control_mean = round(`controlMean', 1)
		local treat = _b[treatment]
		local teffect = 100*(`treat'/`controlMean')
		estadd scalar t_effect = round(`teffect', .01)

		count if e(sample)
		estadd scalar N_ind=r(N)
		
		local t = _b[treatment]/_se[treatment]
		local pvalue = 2*ttail(e(df_r),abs(`t'))
		estadd scalar pvalue = round(`pvalue',.001)
		
		*FDR q-value
		preserve
			gen outcome="`outcome'"
			keep outcome
			duplicates drop
			merge 1:1 outcome using `qval_data',keep(3) nogen
			estadd scalar qval = qval_fdr
		restore

	*COL 2/5: FEMALE
		sum `outcome' if treatment==0 & female==1 [aw=weight]
		local controlMean = r(mean)

		reg `outcome' treatment treatXfemale costXfemale satXfemale $x_controls1 [pw=weight], cluster(psdpsch98)
		count if e(sample) & female==1
		local numobs = r(N)

		lincom treatment + treatXfemale
		local treat_fem = r(estimate)
		local se_fem = r(se)
		cap matrix drop b
		cap matrix drop se
		matrix b = `treat_fem'
		matrix se = `se_fem'
		matrix colnames b = treatment
		matrix colnames se = treatment
		ereturn post b [aw=weight], depname(treatment)
		estadd matrix se
		eststo

		*Add scalars
		estadd scalar control_mean = round(`controlMean', 1)
		local teffect = 100*(`treat_fem'/`controlMean')
		estadd scalar t_effect = round(`teffect', .01)

		local t = `treat_fem'/`se_fem'
		local pvalue = 2*ttail(r(df),abs(`t'))
		estadd scalar pvalue = round(`pvalue',.001)	
		
		estadd scalar N_ind=`numobs'
		
		*FDR q-value
		preserve
			gen outcome="`outcome'"
			gen pval_fem=1
			keep outcome pval_fem
			duplicates drop
			merge 1:1 outcome pval_fem using `qval_gender',keep(3) nogen
			estadd scalar qval = qval_fdr
		restore	

	*COL 3/5: MALE
		sum `outcome' if treatment==0 & female==0 [aw=weight]
		local controlMean = r(mean)
		eststo: reg `outcome' treatment treatXfemale costXfemale satXfemale $x_controls1 [pw=weight], cluster(psdpsch98)

		*Add scalars
		estadd scalar control_mean = round(`controlMean', 1)
		local treat = _b[treatment]
		local teffect = 100*(`treat'/`controlMean')
		estadd scalar t_effect = round(`teffect', .01)

		local t = `treat'/_se[treatment]
		local pvalue = 2*ttail(e(df_r),abs(`t'))
		estadd scalar pvalue = round(`pvalue',.001)
		
		count if e(sample) & female==0
		estadd scalar N_ind=r(N)

		*FDR q-value
		preserve
			gen outcome="`outcome'"
			gen pval_fem=0
			keep outcome pval_fem
			duplicates drop
			merge 1:1 outcome pval_fem using `qval_gender',keep(3) nogen
			estadd scalar qval = qval_fdr
		restore
	
	*COL 4/5: OLDER
		sum `outcome' if treatment==0 & older==1 [aw=weight]
		local controlMean = r(mean)
		
		reg `outcome' older treatment treatXolder costXolder satXolder $x_controls1 [pw=weight], cluster(psdpsch98)
		
		count if e(sample) & older==1
		local numobs = r(N)

		lincom treatment + treatXolder
		local treat_old = r(estimate)
		local se_old = r(se)
		cap matrix drop b
		cap matrix drop se
		matrix b = `treat_old'
		matrix se = `se_old'
		matrix colnames b = treatment
		matrix colnames se = treatment
		ereturn post b [aw=weight], depname(treatment)
		estadd matrix se
		eststo
		
		*Add scalars
		estadd scalar control_mean = round(`controlMean', 1)
		local teffect = 100*(`treat_old'/`controlMean')
		estadd scalar t_effect = round(`teffect', .01)

		local t = `treat_old'/`se_old'
		local pvalue = 2*ttail(r(df),abs(`t'))
		estadd scalar pvalue = round(`pvalue',.001)	
		
		estadd scalar N_ind=`numobs'
		
		*FDR q-value
		preserve
			gen outcome="`outcome'"
			gen pval_old=1
			keep outcome pval_old
			duplicates drop
			merge 1:1 outcome pval_old using `qval_age',keep(3) nogen
			estadd scalar qval = qval_fdr
		restore
	
	*COL 5/5: YOUNGER
		sum `outcome' if treatment==0 & older==0 [aw=weight]
		local controlMean = r(mean)
		eststo: reg `outcome' older treatment treatXolder costXolder satXolder $x_controls1 [pw=weight], cluster(psdpsch98)

		*Add scalars
		estadd scalar control_mean = round(`controlMean', 1)
		local treat = _b[treatment]
		local teffect = 100*(`treat'/`controlMean')
		estadd scalar t_effect = round(`teffect', .01)

		local t = `treat'/_se[treatment]
		local pvalue = 2*ttail(e(df_r),abs(`t'))
		estadd scalar pvalue = round(`pvalue',.001)
		
		count if e(sample) & older==0
		estadd scalar N_ind=r(N)
		
		*FDR q-value
		preserve
			gen outcome="`outcome'"
			gen pval_old=0
			keep outcome pval_old
			duplicates drop
			merge 1:1 outcome pval_old using `qval_age',keep(3) nogen
			estadd scalar qval = qval_fdr
		restore
}

//Export table
# delimit ;

esttab using "$output/KLPS4_E+_earnings_consumption_main_byage_klps4.tex", append
	keep(treatment)
	cells( 	"b( star fmt(%12.0f))"
				"se( par fmt(%12.0f))")
	nomtitles nocons nolz nolines star(* .10 ** .05 *** .01)
	label se noobs collabels(none) longtable fragment nomtitles booktabs posthead(\hline) 
	coeflabels(treatment "\multicolumn{4}{l}{\emph{Panel A: Annual Per-Capita Consumption}}  & \\ Treatment ($\lambda_1$)")
	mlabels("\multicolumn{1}{c}{Full Sample}" "\multicolumn{1}{c}{Female}" "\multicolumn{1}{c}{Male}" "\multicolumn{1}{c}{Older}" "\multicolumn{1}{c}{Younger}")
	stats(control_mean t_effect pvalue qval N_ind,
	labels("\hline Control Mean" "Treatment Effect (\%)" "Treatment p-value" "FDR q-value" "Number Observations") fmt(%12.0g %12.2f %12.3f %12.3f %12.0f))
	substitute("Standard errors in parentheses" " " "\sym{*} \(p<.10\), \sym{**} \(p<.05\), \sym{***} \(p<.01\)" " " "\_" "_")
;
#delimit cr

estimates drop _all

*******************
*PANEL B: EARNINGS*	
*******************

//Add line between panels
texdoc init "$output/KLPS4_E+_earnings_consumption_main_byage_klps4.tex", append force
texdoc write \midrule
texdoc close
	
//Use KLPS-4 data
use "$data/Worms20_Analysis.dta", clear
keep if interview_round==4

//Create interaction variables
gen treatXfemale = treatment * female
gen costXfemale = cost_sharing * female
gen satXfemale = saturation_dm * female

gen treatXolder = treatment * older
gen costXolder = cost_sharing * older
gen satXolder = saturation_dm * older
	
*Run regressions
foreach outcome in tot_earn12m_t {

	*COL 1/5: ALL
		sum `outcome' if treatment==0 [aw=weight]
		local controlMean = r(mean)

		eststo: reg `outcome' treatment $x_controls1 [pw=weight], cluster(psdpsch98)

		*Add scalars
		estadd scalar control_mean = round(`controlMean', 1)
		local treat = _b[treatment]
		local teffect = 100*(`treat'/`controlMean')
		estadd scalar t_effect = round(`teffect', .01)
		
		local t = _b[treatment]/_se[treatment]
		local pvalue = 2*ttail(e(df_r),abs(`t'))
		estadd scalar pvalue = round(`pvalue',.001)

		count if e(sample)
		estadd scalar N_ind=r(N)
		
		*FDR q-value
		preserve
			gen outcome="`outcome'"
			keep outcome
			duplicates drop
			merge 1:1 outcome using `qval_data',keep(3) nogen
			estadd scalar qval = qval_fdr
		restore

	*COL 2/5: FEMALE
		sum `outcome' if treatment==0 & female==1 [aw=weight]
		local controlMean = r(mean)

		reg `outcome' treatment treatXfemale costXfemale satXfemale $x_controls1 [pw=weight], cluster(psdpsch98)
		count if e(sample) & female==1
		local numobs=r(N)

		lincom treatment + treatXfemale
		local treat_fem = r(estimate)
		local se_fem = r(se)
		cap matrix drop b
		cap matrix drop se
		matrix b = `treat_fem'
		matrix se = `se_fem'
		matrix colnames b = treatment
		matrix colnames se = treatment
		ereturn post b [aw=weight], depname(treatment)
		estadd matrix se
		eststo

		*Add scalars
		estadd scalar control_mean = round(`controlMean', 1)
		local teffect = 100*(`treat_fem'/`controlMean')
		estadd scalar t_effect = round(`teffect', .01)
		
		local t = `treat_fem'/`se_fem'
		local pvalue = 2*ttail(r(df),abs(`t'))
		estadd scalar pvalue = round(`pvalue',.001)

		estadd scalar N_ind=`numobs'
		
		*FDR q-value
		preserve
			gen outcome="`outcome'"
			gen pval_fem=1
			keep outcome pval_fem
			duplicates drop
			merge 1:1 outcome pval_fem using `qval_gender',keep(3) nogen
			estadd scalar qval = qval_fdr
		restore


	*COL 3/6: MALE
		sum `outcome' if treatment==0 & female==0 [aw=weight]
		local controlMean = r(mean)
		eststo: reg `outcome' treatment treatXfemale costXfemale satXfemale $x_controls1 [pw=weight], cluster(psdpsch98)

		*Add scalars
		estadd scalar control_mean = round(`controlMean', 1)
		local treat = _b[treatment]
		local teffect = 100*(`treat'/`controlMean')
		estadd scalar t_effect = round(`teffect', .01)

		local t = `treat'/_se[treatment]
		local pvalue = 2*ttail(e(df_r),abs(`t'))
		estadd scalar pvalue = round(`pvalue',.001)
		
		count if e(sample) & female==0
		estadd scalar N_ind=r(N)
		
		*FDR q-value
		preserve
			gen outcome="`outcome'"
			gen pval_fem=0
			keep outcome pval_fem
			duplicates drop
			merge 1:1 outcome pval_fem using `qval_gender',keep(3) nogen
			estadd scalar qval = qval_fdr
		restore

	*COL 4/5: OLDER
		sum `outcome' if treatment==0 & older==1 [aw=weight]
		local controlMean = r(mean)

		reg `outcome' older treatment treatXolder costXolder satXolder $x_controls1 [pw=weight], cluster(psdpsch98)
		count if e(sample) & older==1
		local numobs=r(N)

		lincom treatment + treatXolder
		local treat_old = r(estimate)
		local se_old = r(se)
		cap matrix drop b
		cap matrix drop se
		matrix b = `treat_old'
		matrix se = `se_old'
		matrix colnames b = treatment
		matrix colnames se = treatment
		ereturn post b [aw=weight], depname(treatment)
		estadd matrix se
		eststo

		*Add scalars
		estadd scalar control_mean = round(`controlMean', 1)
		local teffect = 100*(`treat_old'/`controlMean')
		estadd scalar t_effect = round(`teffect', .01)
		
		local t = `treat_old'/`se_old'
		local pvalue = 2*ttail(r(df),abs(`t'))
		estadd scalar pvalue = round(`pvalue',.001)

		estadd scalar N_ind=`numobs'
		
		*FDR q-value
		preserve
			gen outcome="`outcome'"
			gen pval_old=1
			keep outcome pval_old
			duplicates drop
			merge 1:1 outcome pval_old using `qval_age',keep(3) nogen
			estadd scalar qval = qval_fdr
		restore

	*COL 5/5: YOUNGER
		sum `outcome' if treatment==0 & older==0 [aw=weight]
		local controlMean = r(mean)
		eststo: reg `outcome' older treatment treatXolder costXolder satXolder $x_controls1 [pw=weight], cluster(psdpsch98)

		*Add scalars
		estadd scalar control_mean = round(`controlMean', 1)
		local treat = _b[treatment]
		local teffect = 100*(`treat'/`controlMean')
		estadd scalar t_effect = round(`teffect', .01)

		local t = `treat'/_se[treatment]
		local pvalue = 2*ttail(e(df_r),abs(`t'))
		estadd scalar pvalue = round(`pvalue',.001)
		
		count if e(sample) & older==0
		estadd scalar N_ind=r(N)
		
		*FDR q-value
		preserve
			gen outcome="`outcome'"
			gen pval_old=0
			keep outcome pval_old
			duplicates drop
			merge 1:1 outcome pval_old using `qval_age',keep(3) nogen
			estadd scalar qval = qval_fdr
		restore
}

//Export table
# delimit ;

	esttab using "$output/KLPS4_E+_earnings_consumption_main_byage_klps4.tex", append
		cells( 	"b( star fmt(%12.0f))"
				"se( par fmt(%12.0f))")
	nomtitles nocons nolz nolines nonumbers star(* .10 ** .05 *** .01)
	label se noobs collabels(none) longtable fragment nomtitles booktabs 
	mgroups(none)
	mlabels(none)
	keep(treatment)
	coeflabel(treatment "\multicolumn{5}{l}{\emph{Panel B: Annual Individual Earnings}} &  \\ Treatment ($\lambda_1$)")
	stats(control_mean  t_effect pvalue qval N_ind,
	labels("\hline Control Mean" "Treatment Effect (\%)" "Treatment p-value" "FDR q-value" "Number Observations") fmt(%12.0g %12.2f %12.3f %12.3f %12.0f))
	substitute("Standard errors in parentheses" " " "\sym{*} \(p<.10\), \sym{**} \(p<.05\), \sym{***} \(p<.01\)" " " "\_" "_")
	;
	
#delimit cr

estimates drop _all

*****************************
*PANEL C: HOUSEHOLD EARNINGS*	
*****************************

//Add line in between panels
texdoc init "$output/KLPS4_E+_earnings_consumption_main_byage_klps4.tex", append force
texdoc write \midrule
texdoc close
	
//Use KLPS-4 da
use "$data/Worms20_Analysis.dta", clear	
keep if interview_round==4
	
//Create interaction variables
gen treatXfemale = treatment * female
gen costXfemale = cost_sharing * female
gen satXfemale = saturation_dm * female

gen treatXolder = treatment * older
gen costXolder = cost_sharing * older
gen satXolder = saturation_dm * older

//Run regressions
foreach outcome in tot_hhearn12m_t  {

	*COL 1/5: FULL SAMPLE
		sum `outcome' if treatment==0 [aw=weight]
		local controlMean = r(mean)

		eststo: reg `outcome' treatment $x_controls1 [pw=weight], cluster(psdpsch98)

		*Add scalars
		estadd scalar control_mean = round(`controlMean', 1)
		local treat = _b[treatment]
		local teffect = 100*(`treat'/`controlMean')
		estadd scalar t_effect = round(`teffect', .01)
		
		local t = _b[treatment]/_se[treatment]
		local pvalue = 2*ttail(e(df_r),abs(`t'))
		estadd scalar pvalue = round(`pvalue',.001)

		count if e(sample)
		estadd scalar N_ind=r(N)

	*COL 2/5: FEMALE
		sum `outcome' if treatment==0 & female==1 [aw=weight]
		local controlMean = r(mean)

		reg `outcome' treatment treatXfemale costXfemale satXfemale $x_controls1 [pw=weight], cluster(psdpsch98)
		count if e(sample) & female==1
		local numobs=r(N)

		lincom treatment + treatXfemale
		local treat_fem = r(estimate)
		local se_fem = r(se)
		cap matrix drop b
		cap matrix drop se
		matrix b = `treat_fem'
		matrix se = `se_fem'
		matrix colnames b = treatment
		matrix colnames se = treatment
		ereturn post b [aw=weight], depname(treatment)
		estadd matrix se
		eststo

		*Add scalars
		estadd scalar control_mean = round(`controlMean', 1)
		local teffect = 100*(`treat_fem'/`controlMean')
		estadd scalar t_effect = round(`teffect', .01)
		
		local t = `treat_fem'/`se_fem'
		local pvalue = 2*ttail(r(df),abs(`t'))
		estadd scalar pvalue = round(`pvalue',.001)

		estadd scalar N_ind=`numobs'

	*COL 3/6: MALE
		sum `outcome' if treatment==0 & female==0 [aw=weight]
		local controlMean = r(mean)
		eststo: reg `outcome' treatment treatXfemale costXfemale satXfemale $x_controls1 [pw=weight], cluster(psdpsch98)

		*Add scalars
		estadd scalar control_mean = round(`controlMean', 1)
		local treat = _b[treatment]
		local teffect = 100*(`treat'/`controlMean')
		estadd scalar t_effect = round(`teffect', .01)

		local t = `treat'/_se[treatment]
		local pvalue = 2*ttail(e(df_r),abs(`t'))
		estadd scalar pvalue = round(`pvalue',.001)
		
		count if e(sample) & female==0
		estadd scalar N_ind=r(N)
		
	* COL 4/5: OLDER
		sum `outcome' if treatment==0 & older==1 [aw=weight]
		local controlMean = r(mean)

		reg `outcome' older treatment treatXolder costXolder satXolder $x_controls1 [pw=weight], cluster(psdpsch98)
		count if e(sample) & older==1
		local numobs=r(N)

		lincom treatment + treatXolder
		local treat_old = r(estimate)
		local se_old = r(se)
		cap matrix drop b
		cap matrix drop se
		matrix b = `treat_old'
		matrix se = `se_old'
		matrix colnames b = treatment
		matrix colnames se = treatment
		ereturn post b [aw=weight], depname(treatment)
		estadd matrix se
		eststo

		*Add scalars
		estadd scalar control_mean = round(`controlMean', 1)
		local teffect = 100*(`treat_old'/`controlMean')
		estadd scalar t_effect = round(`teffect', .01)
		
		local t = `treat_old'/`se_old'
		local pvalue = 2*ttail(r(df),abs(`t'))
		estadd scalar pvalue = round(`pvalue',.001)

		estadd scalar N_ind=`numobs'

	*COL 5/5: YOUNGER
		sum `outcome' if treatment==0 & older==0 [aw=weight]
		local controlMean = r(mean)
		eststo: reg `outcome' older treatment treatXolder costXolder satXolder $x_controls1 [pw=weight], cluster(psdpsch98)

		*Add scalars
		estadd scalar control_mean = round(`controlMean', 1)
		local treat = _b[treatment]
		local teffect = 100*(`treat'/`controlMean')
		estadd scalar t_effect = round(`teffect', .01)

		local t = `treat'/_se[treatment]
		local pvalue = 2*ttail(e(df_r),abs(`t'))
		estadd scalar pvalue = round(`pvalue',.001)
		
		count if e(sample) & older==0
		estadd scalar N_ind=r(N)
}

//Export table

# delimit ;

	esttab using "$output/KLPS4_E+_earnings_consumption_main_byage_klps4.tex", append
		cells( 	"b( star fmt(%12.0f) )"
				"se( par fmt(%12.0f) )")
	nomtitles nocons nolz nolines nonumbers star(* .10 ** .05 *** .01)
	label se noobs collabels(none) longtable fragment nomtitles booktabs postfoot("\bottomrule")
	mgroups(none)
	mlabels(none)
	keep(treatment)
	coeflabels(treatment "\multicolumn{4}{l}{\emph{Panel C: Annual Per-Capita Household Earnings}} & \\ Treatment ($\lambda_1$)")
	stats(control_mean  t_effect pvalue N_ind ,
	labels("\hline Control Mean" "Treatment Effect (\%)" "Treatment p-value" "Number Observations") fmt(%12.0g %12.2f %12.3f %12.0f))
	substitute("Standard errors in parentheses" " " "\sym{*} \(p<.10\), \sym{**} \(p<.05\), \sym{***} \(p<.01\)" " " "\_" "_")
	;
	
#delimit cr

estimates drop _all
