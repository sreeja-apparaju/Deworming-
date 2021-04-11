 
 * Filename: Heterogeneity_summarystats.do
 * Table: Appendix Table S10
 * Description: This do file creates the table which shows baseline summary stats,
 *		health outcomes, and education and labor market outcomes summary stats

********************************************************************************

clear all
set more off
set maxvar 10000

//Initialize Latex file
	texdoc init "$output/KLPS4_E+_heterogeneity_summarystats.tex", replace force
	texdoc write \\ \toprule
	texdoc close
	
*********************************
*PANEL A: BASELINE SUMMARY STATS*
*********************************

//Use data
use "$data/Worms20_Heterogeneity.dta",clear

//Column 1 & 2: Female and Male
local outcomes age_1998 any_moderate_who z_intensity_who parents_education years_treat 
local modnames age mod_heavy z_int parents_edu years_treat

local genders female male

foreach gender of local genders {	
	*Create separate matrices to store results for females & males
	if "`gender'" == "female" {
		local mat_num m1
	}
	else if "`gender'" == "male" {
		local mat_num m2
	}
	
	local i 0
	cap matrix drop `mat_num'
	foreach outcome of local outcomes {
		local ++i
		local modelname: word `i' of `modnames'
		*Infection rates, parent's education, and age at baseline uses full sample data 
		if "`outcome'" == "any_moderate_who" | "`outcome'" == "z_intensity_who" | "`outcome'" == "parents_education" | "`outcome'" == "age_1998" {
			use "$data/Worms20_Heterogeneity.dta",clear
			keep if `outcome'!=. & female!=.
			keep pupid `outcome' female male klps_popweight
			duplicates drop
			
			duplicates tag pupid,gen(dup)
			assert dup!=1
			
			summ `outcome' if `gender' == 1 [aw=klps_popweight]
			matrix tmp = (r(mean))
			matrix colnames tmp = `modelname'
			matrix `mat_num' = nullmat(`mat_num'), tmp
		}
		*Years of assumed free deworming uses KLPS-4 respondents only
		else if "`outcome'" == "years_treat" {
			use "$data/Worms20_Heterogeneity.dta",clear

			summ `outcome' if `gender'==1 & treatment==0 & interview_round==4 [aw=weight]
			matrix tmp = (r(mean))
			matrix colnames tmp = `modelname'
			matrix `mat_num' = nullmat(`mat_num'), tmp
		}		
	}
}

//Column 3 & 6: Difference between genders and samples
local outcomes age_1998 any_moderate_who z_intensity_who parents_education years_treat 

local groups female older

foreach group of local groups {
	if "`group'" == "female" {
		local mat_name = "A"
	}
	else if "`group'" == "older" {
		local mat_name = "B"
	}
	local i 0
	foreach outcome of local outcomes {
		local ++i
		if "`outcome'" == "any_moderate_who" | "`outcome'" == "z_intensity_who"  | "`outcome'" == "parents_education" | "`outcome'" == "age_1998" {
			use "$data/Worms20_Heterogeneity.dta",clear
			keep if `outcome'!=. & `group'!=.
			keep pupid `outcome' `group' klps_popweight
			duplicates drop
			
			duplicates tag pupid,gen(dup)
			assert dup!=1
			
			eststo model`i': reg `outcome' `group' [pw=klps_popweight]
		}
		else if "`outcome'" == "years_treat" {
			use "$data/Worms20_Heterogeneity.dta",clear

			eststo model`i': reg `outcome' `group' if interview_round==4 & treatment==0 [pw=weight]
		}
	}

	esttab, se nostar keep(`group')
	matrix `mat_name' = r(coefs)
	eststo clear
}

//Column 4 & 5: Older than 12 at baseline and 12 or Younger at baseline
local outcomes age_1998 any_moderate_who z_intensity_who parents_education years_treat 
local modnames age mod_heavy z_int parents_edu years_treat

local samples older younger

foreach sample of local samples {	
	if "`sample'" == "older" {
		local mat_num m3
	}
	else if "`sample'" == "younger" {
		local mat_num m4
	}
		
	local i 0
	cap matrix drop `mat_num'
	foreach outcome of local outcomes {
		local ++i
		local modelname: word `i' of `modnames'
		if "`outcome'" == "any_moderate_who" | "`outcome'" == "z_intensity_who" | "`outcome'" == "parents_education" | "`outcome'" == "age_1998" {
			use "$data/Worms20_Heterogeneity.dta",clear
			keep if `outcome'!=. & older!=.
			keep pupid `outcome' older younger klps_popweight
			duplicates drop
			
			duplicates tag pupid,gen(dup)
			assert dup!=1
			
			summ `outcome' if `sample' == 1 [aw=klps_popweight]
			matrix tmp = (r(mean))
			matrix colnames tmp = `modelname'
			matrix `mat_num' = nullmat(`mat_num'), tmp
		}
		else if "`outcome'" == "years_treat" {
			use "$data/Worms20_Heterogeneity.dta",clear

			summ `outcome' if `sample' == 1 & treatment==0 & interview_round==4 [aw=weight]
			matrix tmp = (r(mean))
			matrix colnames tmp = `modelname'
			matrix `mat_num' = nullmat(`mat_num'), tmp
		}	
	}
}

//Column 1
ereturn post m1
eststo female

//Column 2
ereturn post m2
eststo male

//Column 3 - difference between females & males
local models : coleq A
local models : list uniq models
local nummods : list sizeof models
local modnames age mod_heavy z_int parents_edu years_treat

cap matrix drop b
cap matrix drop se
forval i=1/`nummods' {
	local modelname: word `i' of `modnames'	
	matrix tmp = A[1, 2*`i'-1]
    if tmp[1,1]<. {
		*report coefficient estimate
	    matrix colnames tmp = `modelname'
		matrix b = nullmat(b), tmp
		matrix tmp[1,1] = A[1, 2*`i']
		matrix se = nullmat(se), tmp
	}
}
ereturn post b
estadd matrix se
eststo fem_male_dif
esttab, se mtitle noobs

//Column 4
ereturn post m3
eststo older

//Column 5
ereturn post m4
eststo younger

//Column 6 - difference between older and younger
local models : coleq B
local models : list uniq models
local nummods : list sizeof models
local modnames age mod_heavy z_int parents_edu years_treat

cap matrix drop b
cap matrix drop se
forval i=1/`nummods' {
	local modelname: word `i' of `modnames'	
	matrix tmp = B[1, 2*`i'-1]
    if tmp[1,1]<. {
		*report coefficient estimate
	    matrix colnames tmp = `modelname'
		matrix b = nullmat(b), tmp
		matrix tmp[1,1] = B[1, 2*`i']
		matrix se = nullmat(se), tmp
	}
}
ereturn post b
estadd matrix se
eststo old_young_dif

esttab, se mtitle noobs

//Output Table
# delimit ;
esttab using "$output/KLPS4_E+_heterogeneity_summarystats.tex", append 
	order(age mod_heavy z_int parents_edu years_treat)
	cells(b(star fmt(%12.3f) keep(mod_heavy z_int)) 
	se(par fmt(%12.3f) keep(mod_heavy z_int)) 
	b(star fmt(%12.2f) keep(age parents_edu years_treat)) 
	se(par fmt(%12.2f) keep(age parents_edu years_treat)))
	se nomtitles collabels(none) noobs star(* .10 ** .05 *** .01) fragment
	mlabels("\multicolumn{1}{c}{Female}" "\multicolumn{1}{c}{Male}" "\multicolumn{1}{c}{Difference}" "\multicolumn{1}{c}{Older}" "\multicolumn{1}{c}{Younger}" "\multicolumn{1}{c}{Difference}")
	coeflabels(age "\multicolumn{2}{l}{\emph{Panel A: Baseline Summary Statistics (Full Sample Mean)}} & & & & \\ Age at Baseline (1998)" mod_heavy "Any Moderate-Heavy Infection 1998 (WHO)" z_int "Z-Score of Mean Intensity 1998 (WHO)"  
		parents_edu "Average Years of Parents' Education" years_treat "Years of Assigned Deworming - Control Mean" )
	substitute(\_ _)
	
	;

#delimit cr

estimates clear

*********************************
*PANEL B: OUTCOMES SUMMARY STATS*
*********************************

//Add line between panels
texdoc init "$output/KLPS4_E+_heterogeneity_summarystats.tex", append force
texdoc write \midrule
texdoc close

//Column 1 & 2: Female and Male
local outcomes any_moderate_who_1999 any_moderate_who_2001 health_vgood educyr any_secondary wage_job_opening urban chore_hrs childcare_hrs nonfarm_hrs 
local modnames mod_heavy_1999 mod_heavy_2001 health educyr any_secondary social_network urban chores childcare work_hrs 

local genders female male

foreach gender of local genders {	
	*Create separate matrices for females and males to store means
	if "`gender'" == "female" {
		local mat_num m1
	}
	else if "`gender'" == "male" {
		local mat_num m2
	}
	
	local i 0
	cap matrix drop `mat_num'
	foreach outcome of local outcomes {
		local ++i
		local modelname: word `i' of `modnames'
		if "`outcome'" == "any_moderate_who_1999" {
			use "$data/Worm_Infection_Panel.dta",clear
			summ `outcome' if `gender' == 1 & year==1999 & treat_1999==0 [aw=klps_popweight]
			matrix tmp = (r(mean))
			matrix colnames tmp = `modelname'
			matrix `mat_num' = nullmat(`mat_num'), tmp
		}
		else if "`outcome'" == "any_moderate_who_2001" {
			use "$data/Worm_Infection_Panel.dta",clear
			summ `outcome' if `gender' == 1 & year==2001 & treat_2001==0 [aw=klps_popweight]
			matrix tmp = (r(mean))
			matrix colnames tmp = `modelname'
			matrix `mat_num' = nullmat(`mat_num'), tmp
		}		
		else if "`outcome'" == "chore_hrs" | "`outcome'"== "childcare_hrs" | "`outcome'" == "nonfarm_hrs" | "`outcome'" == "health_vgood" | "`outcome'" == "educyr" | "`outcome'" == "any_secondary" {
			use "$data/Worms20_Heterogeneity.dta",clear
			summ `outcome' if `gender' == 1 & treatment==0 [aw=weight]
			matrix tmp = (r(mean))
			matrix colnames tmp = `modelname'
			matrix `mat_num' = nullmat(`mat_num'), tmp
		}
		else if "`outcome'" == "urban" | "`outcome'" == "wage_job_opening" {
			use "$data/Worms20_Heterogeneity.dta",clear
			summ `outcome' if `gender' == 1 & interview_round==4 & treatment==0 [aw=weight]
			matrix tmp = (r(mean))
			matrix colnames tmp = `modelname'
			matrix `mat_num' = nullmat(`mat_num'), tmp
		}
	}
}

//Column 3 & 6: Difference between genders and samples
local outcomes any_moderate_who_1999 any_moderate_who_2001 health_vgood educyr any_secondary wage_job_opening urban chore_hrs childcare_hrs nonfarm_hrs 

local groups female older

foreach group of local groups {
	if "`group'" == "female" {
		local mat_name = "A"
	}
	else if "`group'" == "older" {
		local mat_name = "B"
	}
	local i 0
	foreach outcome of local outcomes {
		local ++i
		if "`outcome'" == "any_moderate_who_1999"  {
			use "$data/Worm_Infection_Panel.dta",clear
			eststo model`i': reg `outcome' `group' if year==1999 & treat_1999==0 [pw=klps_popweight]
		}
		else if "`outcome'" == "any_moderate_who_2001"  {
			use "$data/Worm_Infection_Panel.dta",clear
			eststo model`i': reg `outcome' `group' if year==2001 & treat_2001==0 [pw=klps_popweight]
		}
		else if "`outcome'" == "chore_hrs" | "`outcome'"== "childcare_hrs" | "`outcome'" == "nonfarm_hrs" | "`outcome'" == "health_vgood" | "`outcome'" == "educyr" | "`outcome'" == "any_secondary" {
			use "$data/Worms20_Heterogeneity.dta",clear
			eststo model`i': reg `outcome' `group' if treatment==0 [pw=weight]
		}
		else if "`outcome'" == "urban" | "`outcome'" == "wage_job_opening" {
			use "$data/Worms20_Heterogeneity.dta",clear
			eststo model`i': reg `outcome' `group' if treatment==0 & interview_round==4 [pw=weight]
		}
	}

	esttab, se nostar keep(`group')
	matrix `mat_name' = r(coefs)
	eststo clear
}

//Column 4 & 5: Older than 12 at baseline and 12 or Younger at baseline
local outcomes any_moderate_who_1999 any_moderate_who_2001 health_vgood educyr any_secondary wage_job_opening urban chore_hrs childcare_hrs nonfarm_hrs 
local modnames mod_heavy_1999 mod_heavy_2001 health educyr any_secondary social_network urban chores childcare work_hrs 

local samples older younger

foreach sample of local samples {	
	*Create separate matrices for females and males to store means
	if "`sample'" == "older" {
		local mat_num m3
	}
	else if "`sample'" == "younger" {
		local mat_num m4
	}
		
	local i 0
	cap matrix drop `mat_num'
	foreach outcome of local outcomes {
		local ++i
		local modelname: word `i' of `modnames'
		if "`outcome'" == "any_moderate_who_1999" {
			use "$data/Worm_Infection_Panel.dta",clear
			summ `outcome' if `sample' == 1 & year==1999 & treat_1999==0 [aw=klps_popweight]
			matrix tmp = (r(mean))
			matrix colnames tmp = `modelname'
			matrix `mat_num' = nullmat(`mat_num'), tmp
		}
		else if "`outcome'" == "any_moderate_who_2001" {
			use "$data/Worm_Infection_Panel.dta",clear
			summ `outcome' if `sample' == 1 & year==2001 & treat_2001==0 [aw=klps_popweight]
			matrix tmp = (r(mean))
			matrix colnames tmp = `modelname'
			matrix `mat_num' = nullmat(`mat_num'), tmp
		}		
		else if "`outcome'" == "chore_hrs" | "`outcome'"== "childcare_hrs" | "`outcome'" == "nonfarm_hrs" | "`outcome'" == "health_vgood" | "`outcome'" == "educyr" | "`outcome'" == "any_secondary" {
			use "$data/Worms20_Heterogeneity.dta",clear
			summ `outcome' if `sample' == 1 & treatment==0 [aw=weight]
			matrix tmp = (r(mean))
			matrix colnames tmp = `modelname'
			matrix `mat_num' = nullmat(`mat_num'), tmp
		}
		else if "`outcome'" == "urban" | "`outcome'" == "wage_job_opening" {
			use "$data/Worms20_Heterogeneity.dta",clear
			summ `outcome' if `sample' == 1 & interview_round==4 & treatment==0 [aw=weight]
			matrix tmp = (r(mean))
			matrix colnames tmp = `modelname'
			matrix `mat_num' = nullmat(`mat_num'), tmp
		}
	}
}

//Column 1
ereturn post m1
eststo female

//Column 2
ereturn post m2
eststo male

//Column 3 - difference between females and males
local models : coleq A
local models : list uniq models
local nummods : list sizeof models
local modnames mod_heavy_1999 mod_heavy_2001 health educyr any_secondary social_network urban chores childcare work_hrs 

cap matrix drop b
cap matrix drop se
forval i=1/`nummods' {
	local modelname: word `i' of `modnames'	
	matrix tmp = A[1, 2*`i'-1]
    if tmp[1,1]<. {
		*add coefficient estiamte
	    matrix colnames tmp = `modelname'
		matrix b = nullmat(b), tmp
		matrix tmp[1,1] = A[1, 2*`i']
		matrix se = nullmat(se), tmp
	}
}
ereturn post b
estadd matrix se
eststo fem_male_dif
esttab, se mtitle noobs

//Column 4
ereturn post m3
eststo older

//Column 5
ereturn post m4
eststo younger

//Column 6 - difference between older & younger
local models : coleq B
local models : list uniq models
local nummods : list sizeof models
local modnames mod_heavy_1999 mod_heavy_2001 health educyr any_secondary social_network urban chores childcare work_hrs 

cap matrix drop b
cap matrix drop se
forval i=1/`nummods' {
	local modelname: word `i' of `modnames'	
	matrix tmp = B[1, 2*`i'-1]
    if tmp[1,1]<. {
		*Add coefficient estimate
	    matrix colnames tmp = `modelname'
		matrix b = nullmat(b), tmp
		matrix tmp[1,1] = B[1, 2*`i']
		matrix se = nullmat(se), tmp
	}
}
ereturn post b
estadd matrix se
eststo old_young_dif

esttab, se mtitle noobs


//Output Table
# delimit ;
esttab using "$output/KLPS4_E+_heterogeneity_summarystats.tex", append 
	keep(mod_heavy_1999 mod_heavy_2001 health)
	order(mod_heavy_1999 mod_heavy_2001 health)
	cells(b(star fmt(%12.3f) keep(mod_heavy_1999 mod_heavy_2001)) 
	se(par fmt(%12.3f) keep(mod_heavy_1999 mod_heavy_2001)) 
	b(star fmt(%12.2f) keep(health)) 
	se(par fmt(%12.2f) keep(health)))
	se nomtitles nolines nonumbers collabels(none) noobs star(* .10 ** .05 *** .01) fragment
	mgroups(none)
	mlabels(none)
	coeflabels(mod_heavy_1999 "\multicolumn{3}{l}{\emph{Panel B: Health Outcomes Summary Statistics (Control Mean)}}  & & & \\ Any Moderate-Heavy Infection 1999 (WHO)" mod_heavy_2001 "Any Moderate-Heavy Infection 2001 (WHO)" 
		health "Indicator for Self-Perceived Health Very Good")
	substitute(\_ _)
	
	;

#delimit cr


************************************************************
*PANEL C: EDUCATION AND LABOR MARKET OUTCOMES SUMMARY STATS*
************************************************************

//Add line between panels
texdoc init "$output/KLPS4_E+_heterogeneity_summarystats.tex", append force
texdoc write \midrule
texdoc close

*Panel C outcomes were estimated in Panel B above

//Output Table
# delimit ;
esttab using "$output/KLPS4_E+_heterogeneity_summarystats.tex", append 
	keep(educyr any_secondary social_network urban chores childcare work_hrs)
	order(educyr any_secondary social_network urban chores childcare work_hrs)
	cells(b(star fmt(%12.2f) keep(educyr any_secondary social_network urban)) 
	se(par fmt(%12.2f) keep(educyr any_secondary social_network urban))
	b(star fmt(%12.1f) keep(chores childcare work_hrs))
	se(par fmt(%12.1f) keep(chores childcare work_hrs)))
	se nomtitles nolines nonumbers collabels(none) noobs star(* .10 ** .05 *** .01) fragment
	mgroups(none)
	mlabels(none)
	coeflabels(educyr "\multicolumn{4}{l}{\emph{Panel C: Education and Labor Market Outcomes Summary Statistics (Control Mean)}}  & & \\ Years of Education by 2011" 
		educyr "Years of Education by 2011" any_secondary "Indicator for Any Secondary School by 2011"
		social_network "Learned of Any Job Through Primary Classmate"
		urban "Indicator for Urban Residence" work_hrs "Hours Worked - Non-Agriculture"
		chores "Chore Hours" childcare "Childcare Hours" )
	substitute(\_ _)
	
	;

#delimit cr

estimates clear
