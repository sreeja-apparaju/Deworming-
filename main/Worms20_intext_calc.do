 
 * Filename: Worms20_intext_calc.do
 * Description: This do file calculates all of the in-text numbers of 
 *		Hamory et al. (2020)
********************************************************************************

clear
clear matrix
clear mata
set more off
set mem 750m
set matsize 800
set maxvar 20000
cap log close


**Line 168/169: In all, 86% of the KLPS sample was surveyed at least once during 
**	the 10, 15 or 20 year rounds.
	log using "$output/percent_ever_surveyed.log", replace
	
	use "$data/Worms20_Attrition.dta", clear
	gen surveyed_once = 1 if surveyed==1 | done_i_klps3 ==1 | done_e_klps3==1
	bys pupid: egen surveyed_max = max(surveyed_once)
	replace surveyed_max = 0 if surveyed_max==.
	keep pupid surveyed_max
	duplicates drop
	tab surveyed_max //86%
	
	cap log close

**Footnote: The hypothesis that differential age effects were driven by school 
**	enrollment patterns led us to postulate in the PAP that there would be only 
**	minimal age differences in impacts by KLPS-4, as only 3% of the sample was 
**	still enrolled in school then.
	clear all
	log using "$output/percent_currently_in_school.log", replace

	use "$data/Worms20_Analysis.dta",clear
	keep if interview_round==4
	summ in_school_klps4 [aw=weight] //1.2% currently in school
	
	cap log close

**Line 328-331: The data indicate that 70% of agricultural activities are in 
**	fact conducted jointly with others, making it challenging to confidently assess 
**	individual agricultural productivity; this is a well-known concern in development economics.
	clear all
	log using "$output/agriculture_with_hhmembers.log",replace
	
	use "$data/Worms20_Analysis.dta",clear
	summ hhmember_work_ag if farm_work_12m!=0  [aw=weight] //70% of households (where the focus respondent worked in ag. in the last 12 months) have at least 1 other hh member working in this agricultural activity

	cap log close
	
**Footnote: If the FDR adjustment is carried out across the six lambda1 coefficient 
**	estimates in columns 4-5 across the three panels, all three estimates for the
**	older subgroup are significant with q-value < 0.05.
	clear all
	log using "$output/FDRadjustment_olderyounger.log", replace

	//set empty tempfile to save pvals for FDR adjustment
	gen outcome=""
	gen pval=.
	tempfile pval_age
	save `pval_age'

	//Use data
	use "$data/Worms20_Analysis.dta",clear

	//Create interaction terms with older/younger
	foreach var of varlist older younger {
		gen treatX`var' = treatment * `var'
		gen costX`var' = cost_sharing * `var'
		gen satX`var' = saturation_dm * `var'
	}

	tempfile all_data
	save `all_data'

	//Define outcomes (consumption, earnings, and hh earnings)
	local outcomes_FDR tot_cnsp_t tot_earn12m_t tot_hhearn12m_t

	foreach outcome of local outcomes_FDR {
			
		//Older
			use `all_data',clear
			drop if older==.
			
			if "`outcome'" == "tot_hhearn12m_t" {	
				eststo: reg `outcome' treatment treatXyounger costXyounger satXyounger younger $x_controls1 [pw=weight], cluster(psdpsch98)
			}
			else {
				eststo: reg `outcome' treatment treatXyounger costXyounger satXyounger younger $x_controls_panel [pw=weight], cluster(psdpsch98)
			}
			
			local t = _b[treatment]/_se[treatment]
			local pvalue = 2*ttail(e(df_r),abs(`t'))
				
			*Saving all p-values in dataset for FDR adjustment
			preserve
				gen outcome = "`outcome'"
				gen pval = `pvalue'
				gen pval_old = 1
				
				keep outcome pval pval_old
				duplicates drop
				
				append using `pval_age'
				save `pval_age',replace
			restore

		//Younger
			use `all_data',clear
			drop if older==.
			
			if "`outcome'" == "tot_hhearn12m_t" {
				eststo: reg `outcome' treatment treatXolder costXolder satXolder older $x_controls1 [pw=weight], cluster(psdpsch98)
			}
			else {
				eststo: reg `outcome' treatment treatXolder costXolder satXolder older $x_controls_panel [pw=weight], cluster(psdpsch98)
			}
			
			local t = _b[treatment]/_se[treatment]
			local pvalue = 2*ttail(e(df_r),abs(`t'))
				
			*Saving all p-values in dataset for FDR adjustment
			preserve
				gen outcome = "`outcome'"
				gen pval = `pvalue'
				gen pval_old = 0
				
				keep outcome pval pval_old
				duplicates drop
				
				append using `pval_age'
				save `pval_age',replace
			restore
	}
	use `pval_age',clear

	//Run FDR adjustment
	preserve
		//older & younger
			fdr_adjustment "`pval_age'"
			keep outcome pval_old qval_fdr
			tempfile qval_age
			save `qval_age'
	restore
	use `qval_age',clear //qval for older group is <.05
	
	cap log close

**Line 392-394: Note that roughly one third of urban migrants live in Nairobi, 
**	and many others live in Mombasa or other large cities.
	clear all
	log using "$output/percent_urban_nairobi.log", replace
	
	use "$data/Worms20_Analysis.dta",clear
	keep if interview_round==4
	summ nairobi if urban==1 [aw=weight] //36.3%
	
	cap log close

**Appendix B lines 25-27: There is ample evidence that cost-sharing had a negative 
**	effect on later outcomes: the estimated lambda2t effect has the opposite sign 
**	of the direct lambda1t effect for 19 of the 21 outcomes in this paper (and 43
**	of the 54 pre-specified outcomes), as predicted; this is extremely unlikely
**	to occur by chance (p-value<.001).

	clear all
	log using "$output/lambda1_lambda2_coefficients.log", replace
	
	//Create empty tempfile to store estimates
	gen outcome=""
	gen treat=.
	gen treat_se=.
	gen cost=.
	gen cost_se=.
	gen saturation=.
	gen saturation_se=.
	tempfile all_estimates
	save `all_estimates'
	
	//Use data	
	preserve
		use pupid interview_round wage_job_opening using "$data/Worms20_Heterogeneity.dta",clear
		tempfile social_network
		save `social_network'
	restore
	
	use "$data/Worms20_Analysis.dta",clear
	merge 1:1 pupid interview_round using `social_network',keep(3) nogen

	//Set outcomes 
	local prespecified_outcomes tot_cnsp_t tot_ln_cnsp_t food_cnsp_t food_ln_cnsp_t nonfood_cnsp_t nonfood_ln_cnsp_t meals_eaten ///
			local_payments_t local_payments_ln_t ///
			tot_wealth_t tot_ln_wealth_t wealth_durables_t wealth_ln_durables_t wealth_livestock_t wealth_ln_livestock_t ///
			tot_earn12m_t tot_ln_earn12m_t tot_hrearn12m_t tot_ln_hrearn12m_t ///
			wage_earn12m_t wage_ln_earn12m_t wage_hrearn12m_t wage_ln_hrearn12m_t ///
			self_earn12m_t self_ln_earn12m_t self_hrearn12m_t self_ln_hrearn12m_t ///
			farm_earn12m_t farm_ln_earn12m_t farm_hrearn12m_t farm_ln_hrearn12m_t ///
			tot_hhearn12m_t tot_ln_hhearn12m_t ///
			taxes12m_t taxes_ln_12m_t ///
			taxpay_all12m_t taxpay_ln_all12m_t ///
			tot_hrs tot_ln_hrs nonzero_hrs farm_hrs farm_ln_hrs wage_hrs wage_ln_hrs self_hrs self_ln_hrs nonzero_wageself_hrs ///
			emp_agri emp_fishing emp_manufacturing emp_construction emp_services emp_wsale_retail emp_contractor 
	
	local additional_outcomes nonzero_earn_12m urban nonfarm_hrs emp_ag_fish emp_services_wsale emp_construct_contract chore_hrs childcare_hrs wage_job_opening
	local outcomes `prespecified_outcomes' `additional_outcomes'
		
	//Run regression
	foreach outcome of local outcomes {

		if ("`outcome'"=="local_payments_t" | "`outcome'"=="local_payments_ln_t") {
				eststo: reg `outcome' treatment $x_controls_panel_bribes [pw=weight], cluster(psdpsch98)
		}
		else if ("`outcome'"=="taxpay_all12m_t" | "`outcome'"=="taxpay_ln_all12m_t") {
			eststo: reg `outcome' treatment $x_controls_panel_all_fees [pw=weight], cluster(psdpsch98)
		}
		else if ("`outcome'" == "tot_hhearn12m_t" | "`outcome'" == "tot_ln_hhearn12m_t" ///
			| "`outcome'" == "wage_job_opening" | "`outcome'" == "childcare_hrs" ///
			| "`outcome'" == "tot_wealth_t" | "`outcome'" == "tot_ln_wealth_t" | "`outcome'" == "wealth_durables_t" ///
			| "`outcome'" == "wealth_ln_durables_t" | "`outcome'" == "wealth_livestock_t" | "`outcome'" == "wealth_ln_livestock_t") {
			eststo: reg `outcome' treatment $x_controls1 [pw=weight], cluster(psdpsch98)
		}
		else {
			eststo: reg `outcome' treatment $x_controls_panel [pw=weight], cluster(psdpsch98)
		}

		preserve
			gen outcome = "`outcome'"
			gen treat = _b[treatment]
			gen treat_se = _se[treatment]
			gen cost = _b[cost_sharing]
			gen cost_se = _se[cost_sharing]
			gen saturation = _b[saturation_dm]
			gen saturation_se = _se[saturation_dm]

			keep outcome treat treat_se ///
				cost cost_se  ///
				saturation saturation_se 
			duplicates drop
				
			append using `all_estimates'
			save `all_estimates',replace
		restore
	}	
	use `all_estimates',clear
	
	//Create flag for all outcomes presented in paper
	gen paper_outcome = 1 if inlist(outcome,"tot_cnsp_t","tot_earn12m_t","tot_hhearn12m_t","tot_ln_earn12m_t","wage_earn12m_t")==1
	replace paper_outcome =1 if inlist(outcome,"self_earn12m_t","farm_earn12m_t","nonzero_earn_12m","tot_hrearn12m_t","tot_wealth_t")==1
	replace paper_outcome = 1 if inlist(outcome,"urban","tot_hrs","farm_hrs","nonfarm_hrs","emp_ag_fish")==1
	replace paper_outcome =1 if inlist(outcome,"emp_services_wsale","emp_construct_contract","emp_manufacturing")==1
	replace paper_outcome = 1 if inlist(outcome,"chore_hrs","childcare_hrs","wage_job_opening")==1
	replace paper_outcome = 0 if paper_outcome==.
	
	//Create flag for outcomes presented in this paper
	gen prespecified_outcome =1 
	foreach outcome of local additional_outcomes {
		replace prespecified_outcome = 0 if outcome=="`outcome'"
	}
	
	//Count number of times lambda1 has opposite sign as lambda2
	count if (treat <0 & cost>0 & prespecified_outcome==1) | (treat>0 & cost<0 & prespecified_outcome==1) //44 of 54 prespecified outcomes
	count if (treat <0 & cost>0 & paper_outcome==1) | (treat>0 & cost<0 & paper_outcome==1) //18 of the 21 outcomes presented in this paper

	cap log close
		
**Appendix B lines 35-36: When estimating saturation effects of the proportion of 
**	treatment schools within 4 km (as opposed to 6 km), saturation terms largely 
**	remain insignificant, while treatment effects remain robust.
	
	clear all
	log using "$output/saturation_4km.log", replace
	
	//Create empty tempfile to store estimates and p-values
	gen outcome=""
	gen treat=.
	gen treat_se=.
	gen treat_pval=.
	gen cost=.
	gen cost_se=.
	gen cost_pval=.
	gen saturation=.
	gen saturation_se=.
	gen saturation_pval=.
	tempfile sat4k_estimates
	save `sat4k_estimates'
		
	//Use data	
	use "$data/Worms20_Analysis.dta",clear

	//Set outcomes 
	local outcomes tot_cnsp_t tot_earn12m_t tot_hhearn12m_t tot_ln_earn12m_t wage_earn12m_t self_earn12m_t farm_earn12m_t nonzero_earn_12m tot_hrearn12m_t tot_wealth_t urban tot_hrs farm_hrs nonfarm_hrs emp_ag_fish emp_services_wsale emp_construct_contract emp_manufacturing
	
	//Run regression
	foreach outcome of local outcomes {

		if "`outcome'" == "tot_hhearn12m_t" {
			eststo: reg `outcome' treatment $x_controls1_4k [pw=weight], cluster(psdpsch98)
		}
		else {
			eststo: reg `outcome' treatment $x_controls_panel_4k [pw=weight], cluster(psdpsch98)
		}
		
		*p-value on treatment
		local t_treat = _b[treatment]/_se[treatment]
		local pvalue_treat = 2*ttail(e(df_r),abs(`t_treat'))
		
		*p-value on cost-sharing
		local t_cost = _b[cost_sharing]/_se[cost_sharing]
		local pvalue_cost = 2*ttail(e(df_r),abs(`t_cost'))
		
		*p-value on saturation
		local t_sat = _b[saturation_dm]/_se[saturation_dm]
		local pvalue_sat = 2*ttail(e(df_r),abs(`t_sat'))

		preserve
			gen outcome = "`outcome'"
			gen treat = _b[treatment]
			gen treat_se = _se[treatment]
			gen treat_pval = `pvalue_treat'
			gen cost = _b[cost_sharing]
			gen cost_se = _se[cost_sharing]
			gen cost_pval = `pvalue_cost'
			gen saturation = _b[saturation_dm]
			gen saturation_se = _se[saturation_dm]
			gen saturation_pval = `pvalue_sat'

			keep outcome treat treat_se treat_pval ///
				cost cost_se cost_pval ///
				saturation saturation_se saturation_pval
			duplicates drop
				
			append using `sat4k_estimates'
			save `sat4k_estimates',replace
		restore
	}
	use `sat4k_estimates',clear //treatment term remains robust, while saturation term is largely insignificant

	cap log close
