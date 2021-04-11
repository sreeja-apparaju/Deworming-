
 * Filename: Worms20_MasterDo.do
 * Description: This do file serves as the master do file to generate
 *    all results for the Twenty Year Impacts of Deworming paper.
********************************************************************************

//Preliminaries
clear all
set more off
set maxvar 10000

//Define directory structure
global dir "" //Update to own local directory

* generate folders not included in the replication folder structure
cd "$dir"
cap mkdir temp
cap mkdir logs
cap mkdir output

global domain "$dir/do"
global rawdata "$dir/rawdata"
global data "$dir/data"
global log "$dir/logs"
global temp "$dir/temp"
global table "$dir/output"
global output "$dir/output"

adopath ++ "$dir/ado/ssc"
adopath ++ "$dir/ado"

//Install necessary packages
  local packages keeporder estout lincom lincomest
  foreach p of local packages {
	capture which `p'.ado
	if _rc==111 ssc install `p'
  }

********************************************************************************

//Define global variables (e.g., conversion rates, controls, etc.)
do "$domain/Worms20_Globals.do"

//Run main tables/figures
do "$domain/main/Worms20_master_main.do"
**Note: Figure 1 needs to be run separately in R (Cost_benefit_pooled.R)

//Run appendix tables/figures
do "$domain/appendix/Worms20_master_paper_appendix.do"
**Notes:
	*Figure S1 is created in a Word document and pulls from Appendix Table S1
	*Figure S2 code and data has not been shared for de-identification purposes
	*Figure S4 needs to be run separately in R (Consumption_graph.R, Earnings_graph.R)
	*Table S12 needs to be run separately using R Markdown (Cost_benefit_table_pooled.Rmd)

//Run in-text numbers
do "$domain/main/Worms20_intext_calc.do"
