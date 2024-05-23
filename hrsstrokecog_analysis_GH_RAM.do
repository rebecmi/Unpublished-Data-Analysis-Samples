
/*
Project: DATA ANALYSIS: Cross-National Gender Differences in Post-Stroke Cognitive Function in the United States
Author: Rebecca A. Milan
Version: 05/15/2024
*/

** HRS-HCAP Stroke-Cog Data Analysis Do-File **
* 1) Import data analysis file 
* 2) Descriptives
* 3) Check normality of residuals for adjusted linear regression model 
* 4) Adjusted linear regression analysis
* 5) Effect modification analysis



*****************************************************************************************************   
******************************** 1) Import HRS-HCAP Analysis dataset from personal drive ********************************
*****************************************************************************************************
/*
*set personal file paths (SEGA Laptop)
clear all
set maxvar 30000
cd "/Users/SEGA Research Laptop/OneDrive - Umich"
global data "Documents/Stata"

*/

*set personal file paths (Beckys Laptop)
clear all
set maxvar 32767
cd "HIDDEN"
global data "HIDDEN"

** Import HRS Stroke-Cog Analysis Dataset
use "HIDDEN", clear



*****************************************************************************************************
******************************** 2) Descriptives************************************************
*****************************************************************************************************

********Exposure**********
tab r13stroke_us if eligible == 1 // n=407 individuals with stroke history

********Outcome***********
*General Cognitive Function*
summarize fgcp if eligible == 1 

*Memory*
summ fmem if eligible == 1 

*Executive Function*
summ fexf if eligible == 1 

*Language*
summ flang if eligible == 1 

*Orientation*
summ forient if eligible == 1 

********Effect Modifier********
tab woman if eligible == 1 


********Covariates********
tabstat age if eligible==1, statistics(count min mean SD median IQR)

tab mingrp if eligible == 1

tab us_educ if eligible == 1 

tab r13lbrf_us if eligible == 1 

tabstat wealth4 if eligible == 1, statistics(count p50 IQR) 

tab r13mstat_us if eligible == 1

tab r13smokestat if eligible == 1 

tab r13drinkstat if eligible == 1 

tab obese if eligible == 1 

tab r13hearte if eligible == 1 

tab r13hibpe if eligible == 1 

tab r13diabe if eligible == 1 

*****************************************************************************************************
******************************** 3) Check for Normality of outcome Residuals and multicollinearity **********
*****************************************************************************************************

global regressvars i.r13stroke_us age i.obese i.r13lbrf_us wealth4 i.r13mstat_us i.r13hearte i.r13diabe i.r13hibpe i.mingrp i.r13drinkstat i.r13smokestat i.us_educ


*General Cognitive Function*
regress fgcp $regressvars if eligible == 1
predict predicted_gen
predict residual_gen, residual
predict rstandard_gen, rstandard
histogram residual_gen

*Memory*
regress fmem $regressvars  if eligible == 1
predict predicted_mem
predict residual_mem, residual
predict rstandard_mem, rstandard
histogram residual_mem

*Executive Function*
regress fexf $regressvars if eligible == 1
predict predicted_ex
predict residual_ex, residual
predict rstandard_ex, rstandard
histogram residual_ex

*Language*
regress flang $regressvars if eligible == 1
predict predicted_lang
predict residual_lang, residual
predict rstandard_lang, rstandard
histogram residual_lang
*normal*

*Orientation*
regress forient $regressvars if eligible == 1
predict predicted_or
predict residual_or, residual
predict rstandard_or, rstandard
histogram residual_or

*check for multicollinearity*
regress fgcp $regressvars if eligible == 1
estat vif
*no multicollienarity,keep model

*****************************************************************************************************
******************************** 4) Adjusted Linear Regression***************************************
*****************************************************************************************************

*****Cognitive function: Linear Regression*****

//Stroke history variable

** General
regress fgcp $regressvars if eligible==1 [pweight=r13wtrespe]

** Memory
regress fmem $regressvars if eligible==1 [pweight=r13wtrespe]

** Executive function
regress fexf $regressvars if eligible==1 [pweight=r13wtrespe]

** Language
regress flang $regressvars if eligible==1 [pweight=r13wtrespe]

** Orientation
regress forient $regressvars if eligible==1 [pweight=r13wtrespe]

******************************************************************
*************************Marginsplots*****************************
******************************************************************

//Stroke history var
regress fgcp $regressvars if eligible==1 [pweight=r13wtrespe]
margins r13stroke_us
marginsplot
marginsplot, title("Coefficient Plots of GCF Factor Scores by Stroke History") subtitle ("HRS-HCAP") xtitle ("Has History of Stroke") ytitle ("GCF Factor Score (SD Units)")


//Longitudinal stroke var
regress fgcp $s2regressvars if eligible==1 [pweight=r13wtrespe]
margins stroke_time
marginsplot, title("Coeff. Plots of GCF Factor Scores by Time Since Last Stroke") subtitle ("HRS-HCAP") xtitle ("Time Since Last Stroke") ytitle ("GCF Factor Score (SD Units)") 


//Separated by gender

** Stroke history
global stroke_wo i.r13stroke_us i.woman age i.obese i.r13lbrf_us wealth4 i.r13mstat_us i.r13hearte i.r13diabe i.r13hibpe i.mingrp i.r13drinkstat i.r13smokestat i.us_educ

regress fgcp $stroke_wo if eligible==1 [pweight=r13wtrespe]
margins woman, at(r13stroke_us=(0 1))
marginsplot, title("Coeff. Plots of GCF Factor Scores by Stroke History, Separated by Gender") subtitle("HRS-HCAP") xtitle("Has History of Stroke") ytitle("GCF Factor Score (SD Units)")

** Longitudinal Stroke
global long_wo i.stroke_time i.woman age i.obese i.r13lbrf_us wealth4 i.r13mstat_us i.r13hearte i.r13diabe i.r13hibpe i.mingrp i.r13drinkstat i.r13smokestat i.us_educ

regress fgcp $long_wo if eligible==1 [pweight=r13wtrespe]
margins woman, at(stroke_time=(0 1 2 3 4 5 6))
marginsplot, title("Coeff. Plots of GCF Factor Scores by Time Since Last Stroke, Separated by Gender") subtitle ("HRS-HCAP") xtitle ("Time Since Last Stroke") ytitle ("GCF Factor Score (SD Units)")

*****************************************************************************************************
******************************** 5) Effect modification analysis: gender and stroke******************
*****************************************************************************************************

//Stroke overall

global em_us r13stroke_us##r13cesd_bin age i.obese i.r13lbrf_us wealth4 i.r13mstat_us i.r13hearte i.r13diabe i.r13hibpe i.mingrp i.r13drinkstat i.r13smokestat i.us_educ

global emce_us i.r13cardio_us##i.woman age i.obese i.r13lbrf_us wealth4 i.r13mstat_us i.r13diabe i.r13hibpe i.mingrp i.r13drinkstat i.r13smokestat i.us_educ

//r-squared for all EM models:
*fgcp= .51
*fmem= .35
*fexf= .53
*flang= .31
*forient= .15

*general*
regress fgcp $em_us if eligible==1 [pweight=r13wtrespe]
//r-squared 51%

regress fgcp $emce_us if eligible_2==1 [pweight=r13wtrespe]

*memory*
regress fmem $em_us if eligible==1 [pweight=r13wtrespe]

regress fmem $emce_us if eligible_2==1 [pweight=r13wtrespe]

*Executive function*
regress fexf $em_us if eligible==1 [pweight=r13wtrespe]

regress fexf $emce_us if eligible_2==1 [pweight=r13wtrespe]

*Language*
regress flang $em_us if eligible==1 [pweight=r13wtrespe]

regress flang $emce_us if eligible_2==1 [pweight=r13wtrespe]

*orientation*
regress forient $em_us if eligible==1 [pweight=r13wtrespe]

regress forient $emce_us if eligible_2==1 [pweight=r13wtrespe]

// Time-varying stroke variable 

global em_ust i.stroke_time#i.woman age i.obese i.r13lbrf_us wealth4 i.r13mstat_us i.r13hearte i.r13diabe i.r13hibpe i.mingrp i.r13drinkstat i.r13smokestat i.us_educ

*general*
regress fgcp $em_ust if eligible==1 [pweight=r13wtrespe]

*memory*
regress fmem $em_ust if eligible==1 [pweight=r13wtrespe]

*Executive function*
regress fexf $em_ust if eligible==1 [pweight=r13wtrespe]

*Language*
regress flang $em_ust if eligible==1 [pweight=r13wtrespe]

*orientation*
regress forient $em_ust if eligible==1 [pweight=r13wtrespe]


