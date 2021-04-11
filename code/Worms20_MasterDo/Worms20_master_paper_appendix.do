
 * Filename: Worms20_master_paper_appendix.do
 * Description: This do file runs all of the do files necessary to create
 *		the appendix figures and tables (excluding all those created in R)
 
 *****************************************************************************

clear
clear matrix
clear mata
set more off
set mem 750m
set matsize 800
set maxvar 20000
cap log close

**FIGURE S1: Project Timeline of the Primary School Deworming Program (PSDP) and the Kenya Life Panel Survey (KLPS)
	**Note that this timeline figure is created in a Word document and tracking rates are from Appendix Table S1

**FIGURE S2: Residential location at the time of KLPS-4 E+ Module (2017-2019)
	**Note that data on location of resopndents has been removed for de-identification purposes

**FIGURE S3: Deworming Treatment Effects by Survey Round
	do "$domain/appendix/Consumption_gender_results_for_R.do"
	do "$domain/appendix/Earnings_gender_results_for_R.do"
	**run the following code in R: Consumption_graph.R
	**run the following code in R: Earnings_graph.R

**FIGURE S4: Annual Per-Capita Consumption Treatment Effects by Years of Deworming
	do "$domain/appendix/Consumption_yearstreat_figure.do"

**TABLE S1: Effective Tracking and Survey Rates, Kenya Life Panel Survey (KLPS) Rounds2, 3 and 4
	do "$domain/appendix/Attrition_main.do"

**TABLE S2: Effective Tracking and Survey Rates by Age at Baseline (Older/Younger than12), Kenya Life Panel Survey (KLPS) Rounds 2, 3 and 4
	do "$domain/appendix/Attrition_byage.do"

**TABLE S3: 20 year deworming treatment effects on consumption and earnings, KLPS Round 4
	do "$domain/appendix/Earnings_consumption_hhearn_main_byage_klps4.do"

**TABLE S4: 20 year effects of deworming on Earnings, Labor Supply, Occupation, and Sectoral Choice, KLPS Round 4
	do "$domain/appendix/Earnings_wealth_labor_occchoice_main_byage_klps4.do"

**TABLE S5: 10 to 20 year deworming treatment effects on consumption and earnings including SCY and VocEd Individuals, KLPS Rounds 2-4
	do "$domain/appendix/Earnings_consumption_hhearn_main_byage_scyvoced.do"

**TABLE S6: 10 to 20 year deworming treatment effects for Earnings, Labor Supply, Occupation, and Sectoral Choice including SCY and VocEd Individuals, KLPS Rounds 2,3 and 4
	do "$domain/appendix/Earnings_wealth_labor_occchoice_main_byage_scyvoced.do"

**TABLE S7: 10 to 20 year deworming effects on consumption, earnings, labor supply, occupational choice, and sector, KLPS Rounds 2, 3 and 4
	do "$domain/appendix/Three_panel_main_outcomes.do"

**TABLE S8: Interaction effects between deworming treatment and parents’ average education,by sex, KLPS Rounds 2, 3 and 4
	do "$domain/appendix/Earnings_consump_parentsedu_long.do"

**TABLE S9: Interaction between deworming treatment and age at time of survey
	do "$domain/appendix/Threepanel_earn_consump_5yr_agebuckets_long.do"

**TABLE S10: Summary statistics on heterogeneity by sex and baseline age
	do "$domain/appendix/Heterogeneity_summarystats.do"

**TABLE S11: Heterogeneous Treatment Effects by Gender and Age for Health, Education and Labor Market Outcomes, KLPS Rounds 2,3 and 4
	do "$domain/appendix/Heterogeneity_treateffects.do"

*TABLE S12: Rate of Return and Net Present Value of Child Deworming
	**run the following in R: Cost_benefit_table_pooled.Rmd
