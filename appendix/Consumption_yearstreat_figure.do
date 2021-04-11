
 * Filename: Consumption_yearstreat_figure.do
 * Table: Appendix Figure S4
 * Description: This do file creates the graph which shows the treatment
 *		effect of deworming by years of deworming with histograms of the
 *		years of assumed free deworming by older/younger than 12 at baseline.

********************************************************************************

clear all
set more off

//Set new controls for regressions (excludes treatment & cost sharing as this is incorporated in years_treat)
global x_controls_panel_yearstreat ///
	saturation_dm  ///
	avgtest96 pup_pop demeaned_popT_6k zoneidI2-zoneidI8  ///
	female std98_base_I2-std98_base_I6 month_interviewI2-month_interviewI12 wave2 i.year ///
	voced

//Use pooled data
use "$data/Worms20_Analysis.dta",clear

//Create years of treatment buckets 
gen years_treat_1 = (years_treat==1 | years_treat==2)
gen years_treat_3 = (years_treat==3 | years_treat==4)
gen years_treat_5 = (years_treat==5 | years_treat==6)

//Run regressions and save coefficient estimates 
local outcomes tot_cnsp_t 
foreach outcome of local outcomes {

	local name = "consumption"
	
	summ `outcome' if treatment==0 [aw=weight]
	loc control_mean = `r(mean)'
	loc controlmean_`name'_1 = `r(mean)'
	loc controlmean_`name'_3 = `r(mean)'
	loc controlmean_`name'_5 = `r(mean)'
		
	reg `outcome' years_treat_1 years_treat_3 years_treat_5 older $x_controls_panel_yearstreat [pw=weight], cluster(psdpsch98)

	forvalues x=1(2)5 {
		reg `outcome' years_treat_1 years_treat_3 years_treat_5 older $x_controls_panel_yearstreat [pw=weight], cluster(psdpsch98)

		lincomest years_treat_`x'
		eststo `name'_`x'
		matrix list r(table)
		matrix define `name'_`x' = r(table)
	}
}

//Store estimates in a matrix
matrix all_estimates_old=[consumption_1,consumption_3,consumption_5]
matrix list all_estimates_old

//Transpose matrix
matrix define all_estimates = all_estimates_old'

//Change matrix to variables and output
svmat all_estimates, names(col)
keep b se ll ul 
keep if b!=.
gen model = ""
replace model = "consumption_1" if _n==1 
replace model = "consumption_3" if _n==2
replace model = "consumption_5" if _n==3

//Add variables for % treatment effect and control means
gen control_mean = .

foreach model in consumption_1 consumption_3 consumption_5 {
	replace control_mean = `controlmean_`model'' if model=="`model'"
}

gen treat_effect = (b / control_mean) * 100
gen treat_effect_ll = (ll / control_mean) * 100
gen treat_effect_ul = (ul / control_mean) * 100

//years_treat = 1.5 = 1-2 years assumed free deworming
//years_treat = 3.5 = 3-4 years assumed free deworming
//years_treat = 5.5 = 5-6 years assumed free deworming
gen years_treat = 1.5 if model=="consumption_1" 
replace years_treat = 3.5 if model=="consumption_3" 
replace years_treat = 5.5 if model=="consumption_5" 

tempfile yearstreat_mainoutcomes
save `yearstreat_mainoutcomes'


***********************
*PANEL A - CONSUMPTION*
***********************

use `yearstreat_mainoutcomes',clear

preserve
	keep if (model=="consumption_1" | model=="consumption_3" | model=="consumption_5") 

	local new = _N + 1
	set obs `new'

	replace treat_effect = 0 if treat_effect==.
	replace years_treat = 0 if years_treat==.

	gen x=0
	gen y=0

	graph twoway (connected treat_effect years_treat, sort msize(large)) ///
		(rcap treat_effect_ll treat_effect_ul years_treat) ///
		(scatter y x, yline(0,lwidth(medthick)) msymbol(D) msize(large)) , ///
		legend(off) xsc(r(-.4 6.4)) xscale(off fill) ///
		ylabel(0 10 20 30 40 50 60 70) ytitle("Treatment Effect (%)") ///
		text(65 1.95 "{bf:A:} Estimated Consumption Effects by Assigned Years of Deworming",size(medlarge) justification(right)) ///
		text(-5 2 "T=1-2 Years",size(medium)) ///
		text(7 3.6 "T=3-4 Years",size(medium)) ///
		text(7 5.6 "T=5-6 Years",size(medium)) ///
		text(7 2 "10.8%",size(large)) ///
		text(27 4 "32.1%",size(large)) ///
		text(37 6 "38.7%",size(large)) ///
		scheme(lean2) fysize(60) name(consumption,replace)
restore


	
***************
*PANEL B and C*
***************

use "$data/Worms20_Analysis.dta",clear

*Generate averages by older / treatment status for histogram plots
	summ years_treat if older==1 & treatment==0 [aw=klps_popweight],d
	local years_treat_older_cont = round(`r(mean)',.1) 
	local round_years_treat_older_cont : di %3.1f `years_treat_older_cont'

	summ years_treat if older==1 & treatment==1 [aw=klps_popweight],d
	local years_treat_older_treat = round(`r(mean)',.1)
	local round_years_treat_older_treat : di %3.1f `years_treat_older_treat'

	summ years_treat if older==0 & treatment==0 [aw=klps_popweight],d
	local years_treat_younger_cont = round(`r(mean)',.1) 
	local round_years_treat_younger_cont : di %3.1f `years_treat_younger_cont'
	local years_treat_younger_cont_graph = `years_treat_younger_cont' - 0.1

	summ years_treat if older==0 & treatment==1 [aw=klps_popweight],d
	local years_treat_younger_treat = round(`r(mean)',.1) 
	local round_years_treat_younger_treat : di %3.1f `years_treat_younger_treat'

contract older treatment years_treat if !missing(older,treatment)

egen _percent = pc(_freq), by(older treatment)

gen older_treat = .
replace older_treat = 1 if older==1 & treatment==0
replace older_treat = 2 if older==0 & treatment==0
replace older_treat = 3 if older==1 & treatment==1
replace older_treat = 4 if older==0 & treatment==1

separate _percent, by(older_treat)

//Update years_treat such that treatment + control are side by side in the graph
gen years_treat_minus = years_treat -.2
gen years_treat_plus = years_treat + .2

//Calculate avg years of treatment by older/younger for treatment and control
gen Percent = 65 if older==0
replace Percent = 65 if older==1
gen avg_years_treat_x = .

replace avg_years_treat_x = `years_treat_older_cont' if older==1 & treatment==0
replace avg_years_treat_x = `years_treat_older_treat' if older==1 & treatment==1
replace avg_years_treat_x = `years_treat_younger_cont' if older==0 & treatment==0
replace avg_years_treat_x = `years_treat_younger_treat' if older==0 & treatment==1

//Older histogram 
twoway bar _percent1 years_treat_minus, base(0) barw(0.4) ///
	|| bar _percent3 years_treat_plus, barw(0.4)  ///
ytitle(Percent) xscale(off fill) ///
ylab(0(20)70) legend(off) scheme(lean2) ///
	|| line Percent avg_years_treat_x if older==1 ///
	|| scatter Percent avg_years_treat_x if treatment==0 & older==1, msymbol(circle_hollow) msize(large) ///
	|| scatter Percent avg_years_treat_x if treatment==1 & older==1, msymbol(circle) msize(large) ///
text(80 .6 "{bf:B:} Older than 12 at Baseline",size(medlarge) justification(right)) ///
text(49 `years_treat_older_cont' "C=`round_years_treat_older_cont'",size(medlarge)) ///
text(49 `years_treat_older_treat' "T=`round_years_treat_older_treat'",size(medlarge)) fysize(20) name(older_full_sample,replace)

//Younger histogram
twoway bar _percent2 years_treat_minus, base(0) barw(0.4) ///
	|| bar _percent4 years_treat_plus, barw(0.4)  ///
	ytitle(Percent) xtitle(Years of Free Deworming) ///
	xtic(0/6) xla(0(1)6) ylab(0(20)70) legend(off) scheme(lean2) ///
	|| line Percent avg_years_treat_x if older==0 ///
	|| scatter Percent avg_years_treat_x if treatment==0 & older==0, msymbol(circle_hollow) msize(large) ///
	|| scatter Percent avg_years_treat_x if treatment==1 & older==0, msymbol(circle) msize(large) ///
text(80 .6 "{bf:C:} 12 or Younger at Baseline",size(medlarge) justification(right)) ///
text(49 `years_treat_younger_cont_graph' "C=`round_years_treat_younger_cont'",size(medlarge)) ///
text(49 `years_treat_younger_treat' "T=`round_years_treat_younger_treat'",size(medlarge)) fysize(25) name(younger_full_sample,replace)


****************
*COMBINE GRAPHS*
****************

graph combine consumption older_full_sample younger_full_sample,col(1) xcommon imargin(0 0 -6 -6) scheme(lean2) 
graph export "$output/ConsumptionTreatEffect_YearsTreat.eps",replace


