 
 * Filename: Earnings_wealth_labor_occchoice_main_byage_klps4.do
 * Table: Appendix Table S4
 * Description: This do file creates the table which shows the treatment effect 
 *		on other earnings outcomes, wealth, labor supply, and sectoral choice
 * 		using KLPS-4 cross-sectional data.

********************************************************************************

clear all
set maxvar 10000
set more off
	
//Initialize Latex document
texdoc init "$output/KLPS4_E+_earnings_labor_occchoice_main_byage_klps4.tex", replace force
texdoc write \\ \toprule
texdoc close

//Use KLPS-4 data
use "$data/Worms20_Analysis.dta", clear
keep if interview_round==4

//Create interaction variables
gen treatXfemale = treatment * female
gen costXfemale = cost_sharing * female
gen satXfemale = saturation_dm * female

gen treatXyounger = treatment * younger
gen costXyounger = cost_sharing * younger
gen satXyounger = saturation_dm * younger


*******************
*PANEL A: EARNINGS*
*******************

//Column 1: Pooled other earnings outcomes
local outcomes tot_ln_earn12m_t wage_earn12m_t self_earn12m_t ///
		farm_earn12m_t nonzero_earn_12m tot_hrearn12m_t tot_wealth_t
local i 0
foreach outcome of local outcomes {
	local ++i
	eststo model`i': reg `outcome' treatment $x_controls1 [pw=weight], cluster(psdpsch98)
}

esttab, se nostar keep(treatment)
matrix A = r(coefs)
eststo clear


//Column 2: Pooled other earnings outcomes - MALE
local outcomes tot_ln_earn12m_t wage_earn12m_t self_earn12m_t ///
		farm_earn12m_t nonzero_earn_12m tot_hrearn12m_t tot_wealth_t
local i 0
foreach outcome of local outcomes {
	local ++i
	eststo model`i': reg `outcome' treatment treatXfemale costXfemale satXfemale  $x_controls1 [pw=weight], cluster(psdpsch98)

}

esttab, se nostar keep(treatment)
matrix B = r(coefs)
eststo clear


//Column 3: Pooled other earnings outcomes - OLDER
local outcomes tot_ln_earn12m_t wage_earn12m_t self_earn12m_t ///
		farm_earn12m_t nonzero_earn_12m tot_hrearn12m_t tot_wealth_t
local i 0
foreach outcome of local outcomes {
	local ++i
	eststo model`i': reg `outcome' younger treatment treatXyounger costXyounger satXyounger  $x_controls1 [pw=weight], cluster(psdpsch98)
}

esttab, se nostar keep(treatment)
matrix C = r(coefs)
eststo clear


//Column 4: Pooled mean of dep variable
local outcomes tot_ln_earn12m_t wage_earn12m_t self_earn12m_t ///
		farm_earn12m_t nonzero_earn_12m tot_hrearn12m_t tot_wealth_t
local modnames log_earn wage_earn self_profit farm_profit nonzero hourly_earn wealth
		
local i 0
cap matrix drop m1
foreach outcome of local outcomes {
	local ++i
	local modelname: word `i' of `modnames'
	summ `outcome' if treatment == 0 [aw=weight] 
	matrix tmp = (r(mean))
	matrix colnames tmp = `modelname'
	matrix m1 = nullmat(m1), tmp
}

//Column 5: Number of observations of full sample
local outcomes tot_ln_earn12m_t wage_earn12m_t self_earn12m_t ///
		farm_earn12m_t nonzero_earn_12m tot_hrearn12m_t tot_wealth_t
local modnames log_earn wage_earn self_profit farm_profit nonzero hourly_earn wealth
		
local i 0
cap matrix drop obs
foreach outcome of local outcomes {
	local ++i
	local modelname: word `i' of `modnames'
	count if `outcome'!=.
	matrix tmp=(r(N))
	matrix colnames tmp = `modelname'
	matrix obs = nullmat(obs),tmp
}


** CREATE TABLE

//Column 1
local models : coleq A
local models : list uniq models
local nummods : list sizeof models
local modnames log_earn wage_earn self_profit farm_profit nonzero hourly_earn wealth

cap matrix drop b
cap matrix drop se
forval i=1/`nummods' {
	local modelname: word `i' of `modnames'	
	matrix tmp = A[1, 2*`i'-1]
    if tmp[1,1]<. {
	    matrix colnames tmp = `modelname'
		matrix b = nullmat(b), tmp
		matrix tmp[1,1] = A[1, 2*`i']
		matrix se = nullmat(se), tmp
	}
}
ereturn post b
estadd matrix se
eststo pooled_all
esttab, se mtitle noobs

//Column 2
local models : coleq B
local models : list uniq models
local nummods : list sizeof models
local modnames log_earn wage_earn self_profit farm_profit nonzero hourly_earn wealth

cap matrix drop b
cap matrix drop se
forval i=1/`nummods' {
	local modelname: word `i' of `modnames'	
	matrix tmp = B[1, 2*`i'-1]
    if tmp[1,1]<. {
	    matrix colnames tmp = `modelname'
		matrix b = nullmat(b), tmp
		matrix tmp[1,1] = B[1, 2*`i']
		matrix se = nullmat(se), tmp
	}
}
ereturn post b
estadd matrix se
eststo pooled_male
esttab, se mtitle noobs

//Column 3
local models : coleq C
local models : list uniq models
local nummods : list sizeof models
local modnames log_earn wage_earn self_profit farm_profit nonzero hourly_earn wealth

cap matrix drop b
cap matrix drop se
forval i=1/`nummods' {
	local modelname: word `i' of `modnames'	
	matrix tmp = C[1, 2*`i'-1]
    if tmp[1,1]<. {
	    matrix colnames tmp = `modelname'
		matrix b = nullmat(b), tmp
		matrix tmp[1,1] = C[1, 2*`i']
		matrix se = nullmat(se), tmp
	}
}
ereturn post b
estadd matrix se
eststo pooled_older
esttab, se mtitle noobs

//Column 4
ereturn post m1
eststo pooled_meandepvar

//Column 5
ereturn post obs
eststo pooled_obs


//Output table
# delimit ;
esttab using "$output/KLPS4_E+_earnings_labor_occchoice_main_byage_klps4.tex", append 
	order(log_earn wage_earn self_profit farm_profit nonzero hourly_earn wealth)
	cells(b(star fmt(%12.0f) keep(wage_earn self_profit farm_profit wealth)) 
	se(par fmt(%12.0f) keep(wage_earn self_profit farm_profit wealth)) 
	b(star fmt(%12.2f) keep(log_earn nonzero hourly_earn )) 
	se(par fmt(%12.2f) keep(log_earn nonzero hourly_earn )))
	se nomtitles collabels(none) noobs star(* .10 ** .05 *** .01) fragment
	mgroups("\bettershortstack{Treatment ($\lambda_1$)}" "\bettershortstack{Full Sample}", pattern(1 0 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
	mlabels("\multicolumn{1}{c}{Full Sample}" "\multicolumn{1}{c}{Male}" "\multicolumn{1}{c}{Older}" "\multicolumn{1}{c}{Control Mean}" "\multicolumn{1}{c}{Number Obs.}")
	coeflabels(log_earn "\emph{Panel A: Earnings and Wealth} & & & \\ Log Annual Individual Earnings" wage_earn "Wage Earnings (annual)" self_profit "Self-Employment Profit (annual)" 
		farm_profit "Individual Farming Profit (annual)" nonzero "Non-Zero Earnings" hourly_earn "Hourly Earnings" wealth "Per-Capita Household Wealth" )
	substitute(\_ _)
	
	;

#delimit cr

estimates clear

********************************************************
*PANEL B: LABOR SUPPLY, OCCUPATION, AND SECTORAL CHOICE*
********************************************************

//Add line in between panels
texdoc init "$output/KLPS4_E+_earnings_labor_occchoice_main_byage_klps4.tex", append force
texdoc write \midrule
texdoc close

//Column 1: Pooled other earnings outcomes
local outcomes urban tot_hrs farm_hrs nonfarm_hrs emp_ag_fish emp_services_wsale emp_construct_contract emp_manufacturing 
local i 0
foreach outcome of local outcomes {
	local ++i
	eststo model`i': reg `outcome' treatment $x_controls1 [pw=weight], cluster(psdpsch98)
}

esttab, se nostar keep(treatment)
matrix A = r(coefs)
eststo clear

//Column 2: Pooled other earnings outcomes - MALE
local outcomes urban tot_hrs farm_hrs nonfarm_hrs emp_ag_fish emp_services_wsale emp_construct_contract emp_manufacturing 
local i 0
foreach outcome of local outcomes {
	local ++i
	eststo model`i': reg `outcome' treatment treatXfemale satXfemale costXfemale $x_controls1 [pw=weight], cluster(psdpsch98)
}

esttab, se nostar keep(treatment)
matrix B = r(coefs)
eststo clear

//Column 3: Pooled other earnings outcomes - OLDER
local outcomes urban tot_hrs farm_hrs nonfarm_hrs emp_ag_fish emp_services_wsale emp_construct_contract emp_manufacturing 
local i 0
foreach outcome of local outcomes {
	local ++i
	eststo model`i': reg `outcome' younger treatment treatXyounger satXyounger costXyounger $x_controls1 [pw=weight], cluster(psdpsch98)
}

esttab, se nostar keep(treatment)
matrix C = r(coefs)
eststo clear


//Column 4: Pooled mean of dep variable
local outcomes urban tot_hrs farm_hrs nonfarm_hrs emp_ag_fish emp_services_wsale emp_construct_contract emp_manufacturing 
local modnames urban_res total_hrs agri_hrs nonagri_hrs af_emp swr_emp c_emp manufacturing_emp 
		
local i 0
cap matrix drop m1
foreach outcome of local outcomes {
	local ++i
	local modelname: word `i' of `modnames'
	summ `outcome' if treatment == 0 [aw=weight] 
	matrix tmp = (r(mean))
	matrix colnames tmp = `modelname'
	matrix m1 = nullmat(m1), tmp
}

//Column 5: Number of observations of full sample
local outcomes urban tot_hrs farm_hrs nonfarm_hrs emp_ag_fish emp_services_wsale emp_construct_contract emp_manufacturing 
local modnames urban_res total_hrs agri_hrs nonagri_hrs af_emp swr_emp c_emp manufacturing_emp 
		
local i 0
cap matrix drop obs
foreach outcome of local outcomes {
	local ++i
	local modelname: word `i' of `modnames'
	count if `outcome'!=.
	matrix tmp=(r(N))
	matrix colnames tmp = `modelname'
	matrix obs = nullmat(obs),tmp
}

** CREATE TABLE

//Column 1
local models : coleq A
local models : list uniq models
local nummods : list sizeof models
local modnames	urban_res total_hrs agri_hrs nonagri_hrs af_emp swr_emp c_emp manufacturing_emp 

cap matrix drop b
cap matrix drop se
forval i=1/`nummods' {
	local modelname: word `i' of `modnames'	
	matrix tmp = A[1, 2*`i'-1]
    if tmp[1,1]<. {
	    matrix colnames tmp = `modelname'
		matrix b = nullmat(b), tmp
		matrix tmp[1,1] = A[1, 2*`i']
		matrix se = nullmat(se), tmp
	}
}
ereturn post b
estadd matrix se
eststo pooled_all
esttab, se mtitle noobs


//Column 2
local models : coleq B
local models : list uniq models
local nummods : list sizeof models
local modnames urban_res total_hrs agri_hrs nonagri_hrs af_emp swr_emp c_emp manufacturing_emp 

cap matrix drop b
cap matrix drop se
forval i=1/`nummods' {
	local modelname: word `i' of `modnames'	
	matrix tmp = B[1, 2*`i'-1]
    if tmp[1,1]<. {
	    matrix colnames tmp = `modelname'
		matrix b = nullmat(b), tmp
		matrix tmp[1,1] = B[1, 2*`i']
		matrix se = nullmat(se), tmp
	}
}
ereturn post b
estadd matrix se
eststo pooled_male
esttab, se mtitle noobs

//Column 3
local models : coleq C
local models : list uniq models
local nummods : list sizeof models
local modnames urban_res total_hrs agri_hrs nonagri_hrs af_emp swr_emp c_emp manufacturing_emp 

cap matrix drop b
cap matrix drop se
forval i=1/`nummods' {
	local modelname: word `i' of `modnames'	
	matrix tmp = C[1, 2*`i'-1]
    if tmp[1,1]<. {
	    matrix colnames tmp = `modelname'
		matrix b = nullmat(b), tmp
		matrix tmp[1,1] = C[1, 2*`i']
		matrix se = nullmat(se), tmp
	}
}
ereturn post b
estadd matrix se
eststo pooled_older
esttab, se mtitle noobs

//Column 4
ereturn post m1
eststo pooled_meandepvar

//Column 5
ereturn post obs
eststo pooled_obs


//Output table
# delimit ;
esttab using "$output/KLPS4_E+_earnings_labor_occchoice_main_byage_klps4.tex", append 
	order(urban_res total_hrs agri_hrs nonagri_hrs af_emp swr_emp c_emp manufacturing_emp)
	cells(b(star fmt(%12.2f) keep(total_hrs agri_hrs nonagri_hrs urban_res)) 
	se(par fmt(%12.2f) keep(total_hrs agri_hrs nonagri_hrs urban_res))
	b(star fmt(%12.3f) keep(af_emp swr_emp c_emp manufacturing_emp)) 
	se(par fmt(%12.3f) keep(af_emp swr_emp c_emp manufacturing_emp))) 
	se nomtitles nolines nonumbers collabels(none) noobs star(* .10 ** .05 *** .01) fragment  postfoot("\bottomrule")
	mgroups(none)
	mlabels(none)
	coeflabels(urban_res "\multicolumn{3}{l}{\emph{Panel B: Labor Supply, Occupation, and Sectoral Choice}} & \\ Urban Residence" total_hrs "Total Hours Worked (last 7 days)" agri_hrs "Hours Worked - Agriculture (last 7 days)" nonagri_hrs "Hours Worked - Non-Agriculture (last 7 days)" af_emp "Employed - Agriculture/Fishing" 
		swr_emp "Employed - Services/Wholesale/Retail" c_emp "Employed - Construction/Trade Contractor" manufacturing_emp "Employed - Manufacturing")
	substitute(\_ _)
	
	;


#delimit cr
