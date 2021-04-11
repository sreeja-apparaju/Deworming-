
 * Filename: Attrition_main.do
 * Table: Appendix Table S1 
 * Description: This do file serves as the do file to generate
 *    the multi-panel results for KLPS-2, KLPS-3, and KLPS-4 attrition.

 * Note: The attrition sample are those found during regular tracking and the 
 *		intensive tracking subsample. For more details on the intensive tracking 
 *		approach, see:
 *			Baird, Sarah, Joan Hamory Hicks, and Edward Miguel. (2008). 
 *			“Tracking, Attrition and Data Quality in the Kenyan Life Panel Survey 
 *			Round 1 (KLPS-1)”, University of California CIDER Working Paper #C08-151.
********************************************************************************

clear all
set more off
set maxvar 10000
capture log close

//Define locals
	*Outcome
	local outcomes found dead surveyed 

	*Genders
	local genders full_sample female male

	*Column numbers (genders * 2)
	local num_genders : word count `genders'
	local num_columns 2*`num_genders'

	*Panels
	local panels A B C D

	*Row names
	local outputvar_found "Found"
	local outputvar_dead "Deceased"
	local outputvar_surveyed "Surveyed, among non-deceased"

	*Column names
	local colname_full_sample "All"
	local colname_female "Female"
	local colname_male "Male"


//Initialize latex file
	*Create latex file
	texdoc init "$output/KLPS4_E+_Attrition_main.tex", replace force

	*Write table preamble
	texdoc write \begin{tabular}{l*{`= `num_columns' + 1'}{c}} 

	*Close tex file
	texdoc close  

//Create separate samples by panel (KLPS-4, KLPS-3 I-Mod, KLPS-3 E-Mod, KLPS-2)	
foreach panel of local panels {

	if ("`panel'"=="A") {
		
		*Use KLPS-4 data
		use "$data/Worms20_Attrition.dta", clear
		keep if interview_round==4
		
		*Keep attrition sample
		keep if attrition_sample==1
		
		*Drop SCY treatment and VocEd treatment
		drop if scy_treat==1 | voced_treat_voucher==1
	}

	else if "`panel'"=="D" {
	
		*Use KLPS-2 data
		use "$data/Worms20_Attrition.dta", clear
		keep if interview_round==2
		
		*Keep attrition sample
		keep if attrition_sample==1 
	}
	
	else if ("`panel'"=="B" | "`panel'"=="C") {
	
		*Use KLPS-3 data
		use "$data/Worms20_Attrition.dta", clear
		keep if interview_round==3
		
		*Keep attrition sample
		keep if attrition_sample==1
		
		*Drop VocEd treatment group
		drop if voced_treat_voucher ==1

		if "`panel'"=="B" {
			*Define surveyed variable for KLPS-3 I-Module
			replace surveyed=done_i_klps3
			
		}
		else if "`panel'"=="C" {
			*Define surveyed variable for KLPS-3 E-Module
			replace surveyed=done_e_klps3
						
			*Only keep E-module sample for Panel B.2
			keep if sample_e_klps3==1
		}
	}
	
	gen full_sample = 1
		
	//Generate columns which display control mean	
	foreach outcome of local outcomes {
	
		foreach gender of local genders {

			gen outputvar = 1
			
			*Surveyed variable is conditional on being non-deceased
			if "`outcome'"=="surveyed"{
				quietly eststo : xi: reg `outcome' outputvar treatment if (`gender'==1 & dead==0) [pw=weight], cl(psdpsch98) nocons
				
				*Add number of surveyed
				count if surveyed==1 & `gender'==1 & dead==0
				estadd scalar num_surveyed = `r(N)'
			}
			else{
				quietly eststo : xi: reg `outcome' outputvar treatment if `gender'==1 [pw=weight], cl(psdpsch98) nocons
			}
			
			drop outputvar
		}

	//Run regressions on treatment - control differences	
		foreach gender of local genders {

			gen outputvar = treatment
			if "`outcome'"=="surveyed"{
				quietly eststo : xi: reg `outcome' outputvar if (`gender'==1 & dead==0) [pw=weight], cl(psdpsch98)
			}
			else{
				quietly eststo : xi: reg `outcome' outputvar if `gender'==1 [pw=weight], cl(psdpsch98)
			}
			drop outputvar
		}

		*Get header
		local header "& \multicolumn{`num_genders'}{c}{\bf{Control Mean}} & \multicolumn{`num_genders'}{c}{\bf{Treatment \$-\$ Control (se)}} \\"
		local colnum = 0
		forvalues reps = 1/2 {
			foreach gender of local genders {
				*Increase column number by 1
				local ++colnum
				
				*Append column name
				local header "`header' &{\shortstack{(`colnum') \\ `colname_`gender''}}"
			}
		}

		*Determine if first row of panel, and if so, attach panel header
		local outcome1 : word 1 of `outcomes'
		local outcome3 : word 3 of `outcomes'
		if "`outcome'" == "`outcome1'" & "`panel'"=="A" {
			local mgroups `"\vspace{-.25cm}\\ \toprule \vspace{-.4cm} \\ \vspace{-.4cm}  \\ `header'"'
			local span span
			local posthead "\midrule"
			local obs_count 
		}
		else if "`outcome'" == "`outcome1'" & "`panel'"!="A" & "`panel'"!="C" {
			local mgroups
			local span none
			local posthead "\midrule"
			local obs_count 
		}
		else if "`outcome'" == "`outcome1'" & "`panel'" =="C" {
			local mgroups
			local span none
			local posthead
			local obs_count 
		}
		else if "`outcome'" =="`outcome3'" {
			local mgroups
			local span none
			local posthead 
			local obs_count "num_surveyed , label("Number Surveyed") fmt(%12.0g)"
		}
		else {
			local mgroups
			local span none
			local posthead
			local obs_count
		}

		*Panel and variable labels
		gen outputvar=.
		if "`panel'"=="A" & "`outcome'" == "found" {
			label variable outputvar "\emph{Panel A: KLPS-4 E+ Module (2017-19)} & & & & & & \\ `outputvar_`outcome''"
		}
		else if "`panel'" =="B" & "`outcome'" == "found" {
			label variable outputvar "\emph{Panel B.1: KLPS-3 I Module (2011-14)} & & & & & & \\ `outputvar_`outcome''"
		}
		else if "`panel'" =="C" & "`outcome'" == "found" {
			label variable outputvar "& & & & & & \\ \emph{Panel B.2: KLPS-3 E Module (2011-14)} & & & & & & \\ `outputvar_`outcome''"
		}
		else if "`panel'" =="D" & "`outcome'" == "found" {
			label variable outputvar "\emph{Panel C: KLPS-2 (2007-09)} & & & & & & \\ `outputvar_`outcome''"
		}
		else {
			label variable outputvar "`outputvar_`outcome''"
		}
		
		//Output table
		# delimit ;
			esttab using "$output/KLPS4_E+_Attrition_main.tex", 
			cells ("b(fmt(%12.3f) pattern(1 1 1 0 0 0)) b(star fmt(%12.3f) pattern(0 0 0 1 1 1))" 
					"se(par fmt(%12.3f) pattern(0 0 0 0 0 0)) se(par fmt(%12.3f) pattern(0 0 0 1 1 1))" ) 
				nomtitles nocons nolz nolines star(* .10 ** .05 *** .01)
				label se noobs nonumbers collabels(none) append longtable fragment nomtitles booktabs
				keep(outputvar)
				mgroups(`"`mgroups'"', `span')
				posthead(`posthead')
				stats(`obs_count')
			;
		#delimit cr
		
		*Drop output var (only used for row name)
		drop outputvar
		
		//Clear estimates
		estimates clear
	}
}

//Close out latex file
	texdoc init "$output/KLPS4_E+_Attrition_main.tex", append force
	texdoc write \bottomrule
	texdoc write \end{tabular}
	texdoc close
