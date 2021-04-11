 
 * Filename: Earnings_wealth_labor_occchoice_main_byage.do
 * Figure: Figure 2
 * Description: The kernel densities of earnings, consumption, and 
 *		household earnings results

********************************************************************************

clear all
set more off
ssc install grstyle, replace

grstyle init
grstyle set plain, horizontal grid

*************
*CONSUMPTION*
*************

use "$data/Worms20_Analysis.dta", clear
	
graph twoway ///
	(kdensity tot_ln_cnsp_t if ///
		treatment==1 [aw=weight], lc(black) lw(thick) lpattern(solid)) ///
	(kdensity tot_ln_cnsp_t if ///
		treatment==0 [aw=weight], lc(gs10) lw(thick) lpattern(solid)), ///
	scheme(lean2) ///
	text(.35 5.25 "Control",color(gs10) size(vlarge)) ///
	text(.28 9.5 "Treatment",color(black) size(vlarge)) ///
	ylabel(0(.1).5,labsize(large) glcolor(gs13)) ///
	xlabel(-2(2)10,labsize(large) glcolor(gs13)) ///
	graphregion (fcolor(white)) ///
	legend(off) ///
	xscale(range(-2 10)) ///
	xsize(10) ///
	text(.47 1.8 "{bf:A:} Annual Per-Capita Consumption (KLPS 3 & 4)",size(large) justification(right)) ///
	xtitle("") ytitle("Kernel Density",size(large)) fysize(33) name(consumption,replace)

**********
*EARNINGS*
**********
	
graph twoway ///
	(kdensity tot_ln_earn12m_t if ///
		treatment==1 [aw=weight], lc(black) lw(thick) lpattern(solid)) ///
	(kdensity tot_ln_earn12m_t if ///
		treatment==0 [aw=weight], lc(gs10) lw(thick) lpattern(solid)), ///
	scheme(lean2) ///
	ylabel(0(.1).4,labsize(large) glcolor(gs13)) ///
	xlabel(-2(2)10,labsize(large) glcolor(gs13)) ///
	graphregion (fcolor(white)) ///
	legend(off) ///
	xscale(range(-2 10)) ///
	xsize(10) ///
	text(.35 1.35 "{bf:B:} Annual Individual Earnings (KLPS 2, 3 & 4)",size(large) justification(right)) ///
	xtitle("") ytitle("4Kernel Density",size(large)) fysize(33) name(earnings,replace)
	

********************
*HOUSEHOLD EARNINGS*
********************
		
graph twoway ///
	(kdensity tot_ln_hhearn12m_t if ///
		treatment==1 [aw=weight], lc(black) lw(thick) lpattern(solid)) ///
	(kdensity tot_ln_hhearn12m_t if ///
		treatment==0  [aw=weight], lc(gs10) lw(thick) lpattern(solid)), ///
	scheme(lean2) ///
	ylabel(0(.1).3,labsize(large)  glcolor(gs13)) ///
	xlabel(-2(2)10,labsize(large)  glcolor(gs13)) ///
	graphregion (fcolor(white)) ///
	legend(off) ///
	xscale(range(-2 10)) ///
	xsize(10) ///
	text(.27 2 "{bf:C:} Annual Per-Capita Household Earnings (KLPS-4)",size(large) justification(right)) ///
	xtitle("") ytitle("Kernel Density",size(large)) fysize(33) name(hh_earn,replace)
	
	
//combine all 3 graphs into 1	
graph combine consumption earnings hh_earn,col(1) imargin(0 0 -6 -6) scheme(lean2) 	

//output
graph export "$output/KLPS4_KD_fullsample.eps",replace
