 
 * Filename: Threepanel_earn_consump_5yr_agebuckets_long.do
 * Table: Appendix Table S9
 * Description: This do file serves as the do file to generate 
*		the analysis of treatment interacted with dummies for age at time of interview

********************************************************************************

clear all
set more off
cap log close

//Use pooled data
use "$data/Worms20_Analysis.dta",clear

//Create age at time of survey
gen age_at_survey = year - yob
drop if age_at_survey==.

//Create indicators for age buckets
local i = 14
local j = 15
forvalues x=10(5)40 {
	gen age_`x'_`i' = (age_at_survey>=`x' & age_at_survey<`j')
	
	gen treatXage_`x'_`i' = treatment * age_`x'_`i'
	gen costXage_`x'_`i' = cost_sharing * age_`x'_`i'
	gen satXage_`x'_`i' = saturation_dm * age_`x'_`i'
	
	local i = `i' + 5
	local j = `j' + 5

}

drop if older==.
	
//Create older interaction variables
gen treatXolder = treatment * older
gen costXolder = cost_sharing * older
gen satXolder = saturation_dm * older	

//not enough observations, so drop from regression
drop if age_40_44==1
drop if age_10_14==1
drop treatXage_40_44 costXage_40_44 satXage_40_44 age_40_44 treatXage_10_14 costXage_10_14 satXage_10_14 age_10_14
	
//create outcome such that there are two columns per outcome (one with and one without older indicator and interaction terms)
gen tot_cnsp_t_older = tot_cnsp_t
gen tot_earn12m_t_older = tot_earn12m_t
gen tot_hhearn12m_t_older  = tot_hhearn12m_t

local outcomes tot_cnsp_t tot_cnsp_t_older tot_earn12m_t tot_earn12m_t_older tot_hhearn12m_t tot_hhearn12m_t_older
	
//initialize tex document
texdoc init "$table/KLPS4_E+_pooled_earnings_consumption_5yr_agebuckets_long.tex", replace force
texdoc write \\ \toprule
texdoc close

//Save regression estimates
foreach outcome of local outcomes {

	sum `outcome' if treatment==0 [aw=weight]
	local controlMean = r(mean)
		
	//consumption uses panel controls	
	if "`outcome'" == "tot_cnsp_t" | "`outcome'" == "tot_cnsp_t_older" {
		preserve
			//drop if few observations
			drop if age_at_survey<20 
			
			if "`outcome'" == "tot_cnsp_t" {
				eststo: reg `outcome' treatXage_20_24 treatXage_25_29 treatXage_30_34 treatXage_35_39 costXage_20_24 costXage_25_29 costXage_30_34 costXage_35_39 satXage_20_24 satXage_25_29 satXage_30_34 satXage_35_39 age_20_24 age_25_29 age_30_34 $x_controls_panel [pw=weight], cluster(psdpsch98)
			}
			else if "`outcome'" == "tot_cnsp_t_older" {
				eststo: reg `outcome' older treatXolder treatXage_20_24 treatXage_25_29 treatXage_30_34 treatXage_35_39 costXolder costXage_20_24 costXage_25_29 costXage_30_34 costXage_35_39 satXolder satXage_20_24 satXage_25_29 satXage_30_34 satXage_35_39 age_20_24 age_25_29 age_30_34 $x_controls_panel [pw=weight], cluster(psdpsch98)
			}
				
			//count num observations
			count if age_15_19 ==1 & `outcome'!=.
			estadd scalar N_age_15_19=r(N)
			
			count if age_20_24==1 & `outcome'!=.
			estadd scalar N_age_20_24 = r(N)
			
			count if age_25_29==1 & `outcome'!=.
			estadd scalar N_age_25_29 = r(N)
			
			count if age_30_34==1 & `outcome'!=.
			estadd scalar N_age_30_34 = r(N)
			
			count if age_35_39==1
			estadd scalar N_age_35_39 = r(N)
		restore
	}
	//household earnings uses KLPS-4 cross-sectional controls
	else if "`outcome'" == "tot_hhearn12m_t" | "`outcome'" == "tot_hhearn12m_t_older" {
		preserve
			//drop if few observations
			drop if age_at_survey<25
			
			if "`outcome'" == "tot_hhearn12m_t" {
				eststo: reg `outcome'  treatXage_30_34 treatXage_35_39  costXage_30_34 costXage_35_39  satXage_30_34 satXage_35_39 age_30_34 age_35_39 $x_controls1 [pw=weight], cluster(psdpsch98)
			}
			else if "`outcome'" == "tot_hhearn12m_t_older" {
				eststo: reg `outcome' older treatXolder  treatXage_30_34 treatXage_35_39 costXolder  costXage_30_34 costXage_35_39 satXolder  satXage_30_34 satXage_35_39 age_30_34 age_35_39 $x_controls1 [pw=weight], cluster(psdpsch98)
			}
				
			//count num observations
			count if age_15_19 ==1 & `outcome'!=.
			estadd scalar N_age_15_19=r(N)
			
			count if age_20_24==1 & `outcome'!=.
			estadd scalar N_age_20_24 = r(N)
			
			count if age_25_29==1 & `outcome'!=.
			estadd scalar N_age_25_29 = r(N)
			
			count if age_30_34==1 & `outcome'!=.
			estadd scalar N_age_30_34 = r(N)
			
			count if age_35_39==1 & `outcome'!=.
			estadd scalar N_age_35_39 = r(N)	
		restore
	}
	//individual earnings uses panel controls
	else if "`outcome'" == "tot_earn12m_t" | "`outcome'" == "tot_earn12m_t_older" {
		
		if "`outcome'" == "tot_earn12m_t" {
			eststo: reg `outcome' treatXage_15_19 treatXage_20_24 treatXage_25_29 treatXage_30_34 treatXage_35_39 costXage_15_19 costXage_20_24 costXage_25_29 costXage_30_34 costXage_35_39 satXage_15_19 satXage_20_24 satXage_25_29 satXage_30_34 satXage_35_39 age_15_19 age_20_24 age_25_29 age_30_34 $x_controls_panel [pw=weight], cluster(psdpsch98)
		}
		else if "`outcome'" == "tot_earn12m_t_older" {
			eststo: reg `outcome' older treatXolder treatXage_15_19 treatXage_20_24 treatXage_25_29 treatXage_30_34 treatXage_35_39 costXolder costXage_15_19 costXage_20_24 costXage_25_29 costXage_30_34 costXage_35_39 satXolder satXage_15_19 satXage_20_24 satXage_25_29 satXage_30_34 satXage_35_39 age_15_19 age_20_24 age_25_29 age_30_34 $x_controls_panel [pw=weight], cluster(psdpsch98)
		}
		
		//count num observations
		count if age_15_19 ==1 & `outcome'!=.
		estadd scalar N_age_15_19=r(N)
		
		count if age_20_24==1 & `outcome'!=.
		estadd scalar N_age_20_24 = r(N)
		
		count if age_25_29==1 & `outcome'!=.
		estadd scalar N_age_25_29 = r(N)
		
		count if age_30_34==1 & `outcome'!=.
		estadd scalar N_age_30_34 = r(N)
		
		count if age_35_39==1 & `outcome'!=.
		estadd scalar N_age_35_39 = r(N)
	
	}
		
	//add control mean scalar in regression table
	estadd scalar control_mean = round(`controlMean',1)
}

//label variables
label variable treatXolder "Treatment x Older than 12 (at baseline)"
label variable treatXage_15_19 "Treatment x Survey Age 15-19"
label variable treatXage_20_24 "Treatment x Survey Age 20-24"
label variable treatXage_25_29 "Treatment x Survey Age 25-29"
label variable treatXage_30_34 "Treatment x Survey Age 30-34"
label variable treatXage_35_39 "Treatment x Survey Age 35-39"

label variable costXolder "Cost Sharing x Older than 12 (at baseline)"
label variable costXage_15_19 "Cost Sharing x Survey Age 15-19"
label variable costXage_20_24 "Cost Sharing x Survey Age 20-24"
label variable costXage_25_29 "Cost Sharing x Survey Age 25-29"
label variable costXage_30_34 "Cost Sharing x Survey Age 30-34"
label variable costXage_35_39 "Cost Sharing x Survey Age 35-39"

label variable satXolder "Saturation x Older than 12 (at baseline)"
label variable satXage_15_19 "Saturation x Survey Age 15-19"
label variable satXage_20_24 "Saturation x Survey Age 20-24"
label variable satXage_25_29 "Saturation x Survey Age 25-29"
label variable satXage_30_34 "Saturation x Survey Age 30-34"
label variable satXage_35_39 "Saturation x Survey Age 35-39"

label variable older "Indicator for Older than 12 (at baseline)"
label variable age_15_19 "Indicator for Survey Age 15-19"
label variable age_20_24 "Indicator for Survey Age 20-24"
label variable age_25_29 "Indicator for Survey Age 25-29"
label variable age_30_34 "Indicator for Survey Age 30-34"


//export table
# delimit ;
	esttab using "$table/KLPS4_E+_pooled_earnings_consumption_5yr_agebuckets_long.tex",  append
		cells ("b(star fmt(%12.0f))"
				"se(par fmt(%12.0f))")
		order(treatXolder treatXage_15_19 treatXage_20_24 treatXage_25_29 treatXage_30_34 treatXage_35_39 costXolder costXage_15_19 costXage_20_24 costXage_25_29 costXage_30_34 costXage_35_39 satXolder satXage_15_19 satXage_20_24 satXage_25_29 satXage_30_34 satXage_35_39 older age_15_19 age_20_24 age_25_29 age_30_34)
		nocons nolz nolines star(* .10 ** .05 *** .01)
		label se noobs collabels(none) fragment nomtitles booktabs nonumbers posthead(\hline)
		mgroups("\bettershortstack{Annual Per-Capita\\Consumption}" "\bettershortstack{Annual\\Individual Earnings}" "\bettershortstack{Annual Per-Capita\\Household Earnings}", pattern(1 0 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
		mlabels("(1)" "(2)" "(3)" "(4)" "(5)" "(6)")
		keep(treatXolder treatXage_15_19 treatXage_20_24 treatXage_25_29 treatXage_30_34 treatXage_35_39 costXolder costXage_15_19 costXage_20_24 costXage_25_29 costXage_30_34 costXage_35_39 satXolder satXage_15_19 satXage_20_24 satXage_25_29 satXage_30_34 satXage_35_39 older age_15_19 age_20_24 age_25_29 age_30_34)
		stats(control_mean N_age_15_19 N_age_20_24 N_age_25_29 N_age_30_34 N_age_35_39 ,
			labels("\hline Control Mean" "Num. Obs. Survey Age 15-19" "Num. Obs. Survey Age 20-24" "Num. Obs. Survey Age 25-29" "Num. Obs. Survey Age 30-34" "Num. Obs. Survey Age 35-39") fmt(%12.0g %12.0f  %12.0f  %12.0f))
			substitute("Standard errors in parentheses" " "
					"\sym{*} \(p<.10\), \sym{**} \(p<.05\), \sym{***} \(p<.01\)" " " \_ _)
;
#delimit cr

estimates drop _all


