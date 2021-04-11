 
 * Filename: Worms20_Globals.do
 * Description: This do file sets conversation rates, and globals for main
 *		regression specifications.
********************************************************************************

//Setting conversion rates

* Updated exchange rate based on PAP (as in income codes): First convert from KSH to US dollars
* at PPP using the average exchange rate by wave, and then converted to 2017 dollars
global exc_ug_k4w1=1/35.2684 		//average exchange rate for UGX/KES (1/5/2017 - 5/21/2018) = 35.2684;  from fxtop.com
global exc_ug_k4w2=1/37.0597 		//average exchange rate for UGX/KES (6/2/2018 - 6/18/2019) = 37.0597;  from fxtop.com

*Set exchange rate (Kenyan shillings to USD at PPP) - World Bank
global exc_usd_ppp_2006 "23.765"
global exc_usd_ppp_2007 "25.024"
global exc_usd_ppp_2008 "28.266"
global exc_usd_ppp_2009 "31.317"
global exc_usd_ppp_2010 "31.603"
global exc_usd_ppp_2011 "34.298"
global exc_usd_ppp_2012 "36.809"
global exc_usd_ppp_2013 "38.044"
global exc_usd_ppp_2014 "40.35"
global exc_usd_ppp_2015 "43.926"
global exc_usd_ppp_2016 "45.862"
global exc_usd_ppp_2017 "49.773"
global exc_usd_ppp_2018 "50.058"

*Set CPI inflation rate - CUUR0000SA0 (not seasonally adjusted) - data.bls.gov
global cpi_2006 "201.6"
global cpi_2007 "207.342"
global cpi_2008 "215.303"
global cpi_2009 "214.537"
global cpi_2010 "218.056"
global cpi_2011 "224.939"
global cpi_2012 "229.594"
global cpi_2013 "232.957"
global cpi_2014 "236.736"
global cpi_2015 "237.017"
global cpi_2016 "240.007"
global cpi_2017 "245.120"
global cpi_2018 "251.107"


//Defining control variables for regressions
** KLPS-4 cross-sectional controls specification

	*Main regression specification
	global x_controls1 ///
		cost_sharing saturation_dm  ///
		avgtest96 pup_pop demeaned_popT_6k zoneidI2-zoneidI8  ///
		female std98_base_I2-std98_base_I6 month_interviewI2-month_interviewI12 wave2 ///
		voced 
	
	*Regression specification used to easily extract treatment effect for females
	global x_controls1_male ///
		cost_sharing saturation_dm  ///
		avgtest96 pup_pop demeaned_popT_6k zoneidI2-zoneidI8  ///
		male std98_base_I2-std98_base_I6 month_interviewI2-month_interviewI12 wave2 ///
		voced 	
	
	*Regression specification used to estimate saturation effects up to 4km (instead of 6km)
	global x_controls1_4k ///
		cost_sharing saturation_dm_4k  ///
		avgtest96 pup_pop demeaned_popT_4k zoneidI2-zoneidI8  ///
		female std98_base_I2-std98_base_I6 month_interviewI2-month_interviewI12 wave2 ///
		voced 


** Pooled controls specification

	*Main regression specification
	global x_controls_panel ///
		cost_sharing saturation_dm  ///
		avgtest96 pup_pop demeaned_popT_6k zoneidI2-zoneidI8  ///
		female std98_base_I2-std98_base_I6 month_interviewI2-month_interviewI12 wave2 i.year ///
		voced

	*Regression specification used to easily extract treatment effect for females
	global x_controls_panel_male ///
		cost_sharing saturation_dm  ///
		avgtest96 pup_pop demeaned_popT_6k zoneidI2-zoneidI8  ///
		male std98_base_I2-std98_base_I6 month_interviewI2-month_interviewI12 wave2 i.year ///
		voced	

	*Regression specification used to estimate saturation effects up to 4km (instead of 6km)
	global x_controls_panel_4k ///
		cost_sharing saturation_dm_4k  ///
		avgtest96 pup_pop demeaned_popT_4k zoneidI2-zoneidI8  ///
		female std98_base_I2-std98_base_I6 month_interviewI2-month_interviewI12 wave2 i.year ///
		voced
	
	*Regression specification which uses KLPS-3 I-Module data
	global x_controls_panel_bribes ///
		cost_sharing saturation_dm  ///
		avgtest96 pup_pop demeaned_popT_6k zoneidI2-zoneidI8  ///
		female std98_base_I2-std98_base_I6 month_interviewI2_bribes-month_interviewI12_bribes wave2 i.year_bribes ///
		voced

	*Regression specification which uses KLPS-3 I-Module data (and used to easily extract treatment effect for females)
	global x_controls_panel_bribes_male ///
		cost_sharing saturation_dm  ///
		avgtest96 pup_pop demeaned_popT_6k zoneidI2-zoneidI8  ///
		female std98_base_I2-std98_base_I6 month_interviewI2_bribes-month_interviewI12_bribes wave2 i.year_bribes ///
		voced
		
	*Regression specification which uses KLPS-3 I-Module data	
	global x_controls_panel_all_fees ///
		cost_sharing saturation_dm  ///
		avgtest96 pup_pop demeaned_popT_6k zoneidI2-zoneidI8  ///
		female std98_base_I2-std98_base_I6 month_interviewI2_all_fees-month_interviewI12_all_fees wave2 i.year_all_fees ///
		voced
	
	*Regression specification which uses KLPS-3 I-Module data (and used to easily extract treatment effect for females)	
	global x_controls_panel_all_fees_male ///
		cost_sharing saturation_dm  ///
		avgtest96 pup_pop demeaned_popT_6k zoneidI2-zoneidI8  ///
		male std98_base_I2-std98_base_I6 month_interviewI2_all_fees-month_interviewI12_all_fees wave2 i.year_all_fees ///
		voced


	