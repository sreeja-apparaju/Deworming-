
 * Filename: Heterogeneity_treateffects.do
 * Table: Appendix Table S11
 * Description: This do file creates the table which shows health outcomes and
 *		education and labor market outcomes treatment effects

********************************************************************************

clear all
set more off
set maxvar 10000

//Write .tex file for combined baseline stats + treatment control stats
	texdoc init "$output/KLPS4_E+_heterogeneity_treateffects.tex", replace force
	texdoc write \\ \toprule
	texdoc close
	
//Create interaction terms for all outcomes
	use "$data/Worms20_Heterogeneity.dta",clear

	gen treatXfemale = treatment*female
	gen treatXmale = treatment*male
	gen treatXolder = treatment*older
	gen treatXyounger = treatment*younger
	gen costXfemale = cost_sharing*female
	gen costXmale = cost_sharing*male
	gen costXolder = cost_sharing*older
	gen costXyounger = cost_sharing*younger
	gen satXfemale = saturation_dm*female
	gen satXmale = saturation_dm*male
	gen satXolder = saturation_dm*older
	gen satXyounger = saturation_dm*younger	
	
	tempfile other_outcomes
	save `other_outcomes'
	
//Columns 1 & 2: Female & Male 
local outcomes any_moderate_who_1999 z_intensity_who_1999 any_moderate_who_2001 z_intensity_who_2001 health_vgood educyr any_secondary wage_job_opening urban chore_hrs childcare_hrs nonfarm_hrs years_treat
local genders female male

foreach gender of local genders {
	if "`gender'" == "female" {
		local mat_num A
		local opp_gender male
	}
	else if "`gender'" == "male" {
		local mat_num B
		local opp_gender female
	}

	local i 0
	foreach outcome of local outcomes {
		local ++i
			if "`outcome'" == "any_moderate_who_1999" | "`outcome'" == "z_intensity_who_1999" {
				use "$data/Worm_Infection_Panel.dta",clear
				
				//Create interaction terms used in regression
				gen treat_1999Xmale = treat_1999*male
				gen treat_1999Xfemale = treat_1999*female
				
				rename treat_1999 treatment
				
				eststo model`i': reg `outcome' treatment treat_1999X`opp_gender' avgtest96 zoneidI2-zoneidI8 std98_base_I2-std98_base_I6 `opp_gender' pup_pop popT_6k if year==1999 [pw=klps_popweight], cluster(psdpsch98)
			}
			else if "`outcome'" == "any_moderate_who_2001" | "`outcome'" == "z_intensity_who_2001" {
				use "$data/Worm_Infection_Panel.dta",clear
				
				//Create interaction terms used in regression
				gen treat_2001Xmale = treat_2001*male
				gen treat_2001Xfemale = treat_2001*female
				
				rename treat_2001 treatment
				
				eststo model`i': reg `outcome' treatment treat_2001X`opp_gender' avgtest96 zoneidI2-zoneidI8 std98_base_I2-std98_base_I6 `opp_gender' pup_pop popT_6k if year==2001 [pw=klps_popweight], cluster(psdpsch98)
			}
			else if "`outcome'" == "chore_hrs" | "`outcome'" =="health_vgood" | "`outcome'" == "nonfarm_hrs" {
				use `other_outcomes',clear
				
				eststo model`i': reg `outcome' treatment treatX`opp_gender' costX`opp_gender' satX`opp_gender' cost_sharing saturation_dm  avgtest96 pup_pop demeaned_popT_6k zoneidI2-zoneidI8 `opp_gender' std98_base_I2-std98_base_I6 month_interviewI2-month_interviewI12 wave2 voced i.year [pw=weight], cluster(psdpsch98)
			}
			else if "`outcome'" == "educyr" | "`outcome'" == "any_secondary" | "`outcome'" == "childcare_hrs" {
				use `other_outcomes',clear
				
				eststo model`i': reg `outcome' treatment treatX`opp_gender' costX`opp_gender' satX`opp_gender' cost_sharing saturation_dm  avgtest96 pup_pop demeaned_popT_6k zoneidI2-zoneidI8 `opp_gender' std98_base_I2-std98_base_I6 month_interviewI2-month_interviewI12 wave2 voced [pw=weight], cluster(psdpsch98)
			}
			else if "`outcome'" == "urban" | "`outcome'"=="wage_job_opening"  {
				use `other_outcomes',clear
				
				keep if interview_round==4
				
				eststo model`i': reg `outcome' treatment treatX`opp_gender' costX`opp_gender' satX`opp_gender' cost_sharing saturation_dm  avgtest96 pup_pop demeaned_popT_6k zoneidI2-zoneidI8 `opp_gender' std98_base_I2-std98_base_I6 month_interviewI2-month_interviewI12 wave2 voced [pw=weight], cluster(psdpsch98)
			}			
			else if "`outcome'" == "years_treat" {
				use `other_outcomes',clear
				
				keep if interview_round==4
				
				eststo model`i': reg `outcome' treatment treatX`opp_gender' satX`opp_gender' saturation_dm  avgtest96 pup_pop demeaned_popT_6k zoneidI2-zoneidI8 `opp_gender' std98_base_I2-std98_base_I6 month_interviewI2-month_interviewI12 wave2 voced [pw=weight], cluster(psdpsch98)
			}
	}

	esttab, se nostar keep(treatment) stats(control_mean)
	matrix `mat_num' = r(coefs)
	eststo clear
}

//Columns 4 & 5: Older & Younger
local outcomes any_moderate_who_1999 z_intensity_who_1999 any_moderate_who_2001 z_intensity_who_2001 health_vgood educyr any_secondary wage_job_opening urban chore_hrs childcare_hrs nonfarm_hrs years_treat
local samples older younger

foreach sample of local samples {
	if "`sample'" == "older" {
		local mat_num D
		local opp_sample younger
	}
	else if "`sample'" == "younger" {
		local mat_num E
		local opp_sample older
	}

	local i 0
	foreach outcome of local outcomes {
		local ++i
			if "`outcome'" == "any_moderate_who_1999" | "`outcome'" == "z_intensity_who_1999" {
				use "$data/Worm_Infection_Panel.dta",clear
				
				//Create interaction terms used in regression
				gen treat_1999Xolder = treat_1999*older
				gen treat_1999Xyounger = treat_1999*younger
				
				rename treat_1999 treatment
				
				eststo model`i': reg `outcome' treatment treat_1999X`opp_sample' female avgtest96 zoneidI2-zoneidI8 std98_base_I2-std98_base_I6 `opp_sample' pup_pop popT_6k if year==1999 [pw=klps_popweight], cluster(psdpsch98)
			}
			else if "`outcome'" == "any_moderate_who_2001" | "`outcome'" == "z_intensity_who_2001" {
				use "$data/Worm_Infection_Panel.dta",clear
				
				//Create interaction terms used in regression
				gen treat_2001Xolder = treat_2001*older
				gen treat_2001Xyounger = treat_2001*younger
				
				rename treat_2001 treatment
				
				eststo model`i': reg `outcome' treatment treat_2001X`opp_sample' female avgtest96 zoneidI2-zoneidI8 std98_base_I2-std98_base_I6 `opp_sample' pup_pop popT_6k if year==2001 [pw=klps_popweight], cluster(psdpsch98)
			}
			else if "`outcome'" == "chore_hrs" | "`outcome'" =="health_vgood" | "`outcome'" == "nonfarm_hrs" {
				use `other_outcomes',clear
				
				eststo model`i': reg `outcome' treatment treatX`opp_sample' costX`opp_sample' satX`opp_sample' cost_sharing saturation_dm female avgtest96 pup_pop demeaned_popT_6k zoneidI2-zoneidI8 `opp_sample' std98_base_I2-std98_base_I6 month_interviewI2-month_interviewI12 wave2 voced i.year [pw=weight], cluster(psdpsch98)
			}
			else if "`outcome'" == "educyr" | "`outcome'" == "any_secondary" | "`outcome'" == "childcare_hrs" {
				use `other_outcomes',clear
				
				eststo model`i': reg `outcome' treatment treatX`opp_sample' costX`opp_sample' satX`opp_sample' cost_sharing saturation_dm female avgtest96 pup_pop demeaned_popT_6k zoneidI2-zoneidI8 `opp_sample' std98_base_I2-std98_base_I6 month_interviewI2-month_interviewI12 wave2 voced [pw=weight], cluster(psdpsch98)
			}	
			else if "`outcome'" == "urban" | "`outcome'"=="wage_job_opening"  {
				use `other_outcomes',clear
				
				keep if interview_round==4
				
				eststo model`i': reg `outcome' treatment treatX`opp_sample' costX`opp_sample' satX`opp_sample' cost_sharing saturation_dm female avgtest96 pup_pop demeaned_popT_6k zoneidI2-zoneidI8 `opp_sample' std98_base_I2-std98_base_I6 month_interviewI2-month_interviewI12 wave2 voced [pw=weight], cluster(psdpsch98)
			}			
			else if "`outcome'" == "years_treat" {
				use `other_outcomes',clear
				
				keep if interview_round==4
				
				eststo model`i': reg `outcome' treatment treatX`opp_sample' satX`opp_sample' saturation_dm female avgtest96 pup_pop demeaned_popT_6k zoneidI2-zoneidI8 `opp_sample' std98_base_I2-std98_base_I6 month_interviewI2-month_interviewI12 wave2 voced [pw=weight], cluster(psdpsch98)
			}

	}

	esttab, se nostar keep(treatment) stats(control_mean)
	matrix `mat_num' = r(coefs)
	eststo clear
}


//Column 3 & 6: Difference between genders and samples
local outcomes any_moderate_who_1999 z_intensity_who_1999 any_moderate_who_2001 z_intensity_who_2001 health_vgood educyr any_secondary wage_job_opening urban chore_hrs childcare_hrs nonfarm_hrs years_treat
local samples male younger

foreach sample of local samples {
	if "`sample'" == "male" {
		local mat_num C
		local opp_sample = "female"
		local fem_control 
	}
	else if "`sample'" == "younger" {
		local mat_num F
		local opp_sample = "older"
		local fem_control = "female"
	}
	local i 0
	foreach outcome of local outcomes {
		local ++i
			if "`outcome'" == "any_moderate_who_1999" | "`outcome'" == "z_intensity_who_1999" {
				use "$data/Worm_Infection_Panel.dta",clear
			
				rename treat_1999 treatment

				//Create interaction terms used in regression	
				gen treatmentX = treatment*`opp_sample'
								
				eststo model`i': reg `outcome' treatment treatmentX `fem_control' avgtest96 zoneidI2-zoneidI8 std98_base_I2-std98_base_I6 `opp_sample' pup_pop popT_6k if year==1999 [pw=klps_popweight],cluster(psdpsch98)

			}
			else if "`outcome'" == "any_moderate_who_2001" | "`outcome'" == "z_intensity_who_2001" {
				use "$data/Worm_Infection_Panel.dta",clear
				
				rename treat_2001 treatment

				//Create interaction terms used in regression				
				gen treatmentX = treatment*`opp_sample'	
				
				eststo model`i': reg `outcome' treatment treatmentX `fem_control'  avgtest96 zoneidI2-zoneidI8 std98_base_I2-std98_base_I6 `opp_sample' pup_pop popT_6k if year==2001 [pw=klps_popweight], cluster(psdpsch98)
				
			}
			else if "`outcome'" == "chore_hrs" | "`outcome'" == "health_vgood" | "`outcome'" == "nonfarm_hrs" {
				use `other_outcomes',clear
				
				//Create interaction terms used in regression	
				drop treatXfemale treatXmale
				gen treatmentX = treatment*`opp_sample'
				
				eststo model`i': reg `outcome' treatment treatmentX costX`opp_sample' satX`opp_sample' cost_sharing saturation_dm  avgtest96 pup_pop demeaned_popT_6k zoneidI2-zoneidI8 `fem_control' `opp_sample' std98_base_I2-std98_base_I6 month_interviewI2-month_interviewI12 wave2 voced i.year [pw=weight], cluster(psdpsch98)
			}
			else if "`outcome'" == "educyr" | "`outcome'" == "any_secondary" | "`outcome'" == "childcare_hrs" {
				use `other_outcomes',clear
				
				//Create interaction terms used in regression	
				drop treatXfemale treatXmale
				gen treatmentX = treatment*`opp_sample'
				
				eststo model`i': reg `outcome' treatment treatmentX costX`opp_sample' satX`opp_sample' cost_sharing saturation_dm `fem_control' avgtest96 pup_pop demeaned_popT_6k zoneidI2-zoneidI8 `opp_sample' std98_base_I2-std98_base_I6 month_interviewI2-month_interviewI12 wave2 voced [pw=weight], cluster(psdpsch98)
			}
			else if "`outcome'" == "urban" | "`outcome'"=="wage_job_opening"  {
				use `other_outcomes',clear
				
				keep if interview_round==4
				
				//Create interaction terms used in regression	
				drop treatXfemale treatXmale
				gen treatmentX = treatment*`opp_sample'
				
				eststo model`i': reg `outcome' treatment treatmentX costX`opp_sample' satX`opp_sample' cost_sharing saturation_dm `fem_control' avgtest96 pup_pop demeaned_popT_6k zoneidI2-zoneidI8 `opp_sample' std98_base_I2-std98_base_I6 month_interviewI2-month_interviewI12 wave2 voced [pw=weight], cluster(psdpsch98)					
			}
			else if "`outcome'" == "years_treat" {
				use `other_outcomes',clear
				
				keep if interview_round==4
				
				//Create interaction terms used in regression	
				drop treatXfemale treatXmale
				gen treatmentX = treatment*`opp_sample'

				eststo model`i': reg `outcome' treatment treatmentX satX`opp_sample' saturation_dm `fem_control' avgtest96 pup_pop demeaned_popT_6k zoneidI2-zoneidI8 `opp_sample' std98_base_I2-std98_base_I6 month_interviewI2-month_interviewI12 wave2 voced [pw=weight], cluster(psdpsch98)
			}	
		}
	esttab, se nostar keep(treatmentX) stats(control_mean)
	matrix `mat_num' = r(coefs)
	eststo clear
}

**Create table

//Columns 1, 2, 4, 5: Female, Male, Older, Younger
local columns A B C D E F

foreach column of local columns {
	local models : coleq `column'
	local models : list uniq models
	local nummods : list sizeof models
	local modnames	mod_who_1999 z_who_1999 mod_who_2001 z_who_2001 health educyr any_secondary social_network urban chores childcare work_hrs years_treat
	
	cap matrix drop b
	cap matrix drop se
	forval i=1/`nummods' {
		local modelname: word `i' of `modnames'	
		matrix tmp = `column'[1, 2*`i'-1]
		if tmp[1,1]<. {
			matrix colnames tmp = `modelname'
			matrix b = nullmat(b), tmp
			matrix tmp[1,1] = `column'[1, 2*`i']
			matrix se = nullmat(se), tmp
		}
	}
	ereturn post b
	estadd matrix se
	
	if "`column'" == "A" {
		eststo female
		esttab, se mtitle noobs stats(control_mean)
	}
	else if "`column'" == "B" {
		eststo male
		esttab, se mtitle noobs stats(control_mean)
	}
	else if "`column'" == "C" {
		eststo fem_male_dif
		esttab, se mtitle noobs stats(control_mean)
	}	
	else if "`column'" == "D" {
		eststo older
		esttab, se mtitle noobs stats(control_mean)
	}	
	else if "`column'" == "E" {
		eststo younger
		esttab, se mtitle noobs stats(control_mean)
	}	
	else if "`column'" == "F" {
		eststo older_younger_dif
		esttab, se mtitle noobs stats(control_mean)
	}
}

**************************
*PANEL A: HEALTH OUTCOMES*
**************************

# delimit ;
esttab using "$output/KLPS4_E+_heterogeneity_treateffects.tex", append 
	keep(years_treat mod_who_1999 z_who_1999 mod_who_2001 z_who_2001 health)
	order(years_treat mod_who_1999 z_who_1999 mod_who_2001 z_who_2001 health)
	cells(b(star fmt(%12.3f) keep(mod_who_1999 z_who_1999 mod_who_2001 z_who_2001)) 
	se(par fmt(%12.3f) keep(mod_who_1999 z_who_1999 mod_who_2001 z_who_2001))
	b(star fmt(%12.2f) keep(health years_treat))
	se(par fmt(%12.2f) keep(health years_treat)))
	se nomtitles collabels(none) noobs star(* .10 ** .05 *** .01) fragment
	mlabels("\multicolumn{1}{c}{Female}" "\multicolumn{1}{c}{Male}" "\multicolumn{1}{c}{Difference}" "\multicolumn{1}{c}{Older}" "\multicolumn{1}{c}{Younger}" "\multicolumn{1}{c}{Difference}")
	coeflabels(years_treat "\multicolumn{2}{l}{\emph{Panel A: Health Outcomes}} & & & & \\ Years of Assigned Deworming" mod_who_1999 "Any Moderate-Heavy Infection 1999 (WHO)"  z_who_1999 "Z-Score of Mean Intensity 1999 (WHO)" mod_who_2001 "Any Moderate-Heavy Infection 2001 (WHO)" z_who_2001 "Z-Score of Mean Intensity 2001 (WHO)"
		health "Indicator for Self-Perceived Health Very Good" )
	substitute(\_ _)
	
	;


#delimit cr

**********************************************
*PANEL B: EDUCATION AND LABOR MARKET OUTCOMES*
**********************************************

texdoc init "$output/KLPS4_E+_heterogeneity_treateffects.tex", append force
texdoc write \midrule
texdoc close


# delimit ;
esttab using "$output/KLPS4_E+_heterogeneity_treateffects.tex", append
	keep(educyr any_secondary social_network urban chores childcare work_hrs)
	order(educyr any_secondary social_network urban chores childcare work_hrs)
	cells(b(star fmt(%12.2f) keep(educyr any_secondary social_network urban))
	se(par fmt(%12.2f) keep(educyr any_secondary social_network urban))
	b(star fmt(%12.1f) keep(chores childcare work_hrs))
	se(par fmt(%12.1f) keep(chores childcare work_hrs)))
	se nomtitles nolines nonumbers collabels(none) noobs star(* .10 ** .05 *** .01) fragment
	mgroups(none)
	mlabels(none)
	coeflabels(educyr "\multicolumn{4}{l}{\emph{Panel B: Education and Labor Market Outcomes}} & & & & \\ Years of Education by 2011" any_secondary "Indicator for Any Secondary School by 2011" 
		social_network "Learned of Any Job Through Primary Classmate"
		urban "Indicator for Urban Residence" work_hrs "Hours Worked - Non-Agriculture"
		chores "Chore Hours" childcare "Childcare Hours")
	substitute(\_ _)
	
	;


#delimit cr
