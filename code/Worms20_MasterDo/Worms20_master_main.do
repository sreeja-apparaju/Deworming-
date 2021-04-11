 
 * Filename: Worms20_master_main.do
 * Description: This do file runs all the necessary files to create the main
 *		tables and figures 
********************************************************************************

clear
clear matrix
clear mata
set more off
set mem 750m
set matsize 800
set maxvar 20000
cap log close

capture log close

**Table 1: 10 to 20 Year Deworming Treatment Effects on Consumption and Earnings,KLPS Rounds 2, 3 and 4
	do "$domain/main/Earnings_consumption_hhearn_main_byage.do"
	
**Table 2: 10 to 20 Year Deworming Treatment Effects on Earnings, Labor Supply, Occupation, and Sectoral Choice, KLPS Rounds 2, 3 and 4
	do "$domain/main/Earnings_wealth_labor_occchoice_main_byage.do"

**Figure 1: Deworming Costs, Benefits and Rate of Return
	**Run in R: Cost_benefit_pooled.R

**Figure 2: Kernel Densities of (Log) Consumption and Earnings, KLPS Rounds 2, 3 and 4
	do "$domain/main/Consumption_earnings_KD.do"
