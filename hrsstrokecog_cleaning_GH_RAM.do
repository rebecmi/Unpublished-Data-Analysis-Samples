/*
Project: DATA CLEANING: Cross-National Gender Differences in Post-Stroke Cognitive Function in the United States
Author: Rebecca A. Milan
Version: 04/22/2024
*/

** HRS/HRS-HCAP Stroke-Cog Data Cleaning Do-File **
* 1) Import HRS-HCAP mega-harmonized datasets - OMITTED b/c of personal file paths
* 2) Merge with HRS-Longitudinal & HRS-RAND - OMITTED b/c of personal file paths
* 3) Data cleaning/management
* 4) Final analytic sample explanation
   
*****************************************************************************************************
******************************** 3)Data cleaning/Management*****************************************************
*****************************************************************************************************
//covariates to include based on livingston et al. (dementia risk factor article) and Boehme et al. (stroke risk factor article): age i.obese i.educattain_resp i.occup wealth4 i.mstat i.r12hearte i.r12diabe i.raracem i.r12drink i.r12smokev 

************************************
*************Effect Modifier********
************************************
mdesc ragender 
tab ragender
gen woman = ragender 
recode woman 1=0 2=1
tab woman,m 
lab define woman_lab 0 "Man" 1 "Woman"
lab val woman woman_lab 
lab var woman "participant gender"
tab woman ragender,m


************************************
*************EXPOSURE***************
************************************

** STROKE
tab r13stroke,m  // N=3,341
tab r13stroke,m nolab
gen r13stroke_us=r13stroke
recode r13stroke_us .m=. .d=. .r=.
*recode so .m .r and .d = missing since less than 5% of sample
lab def stroke_lab 0 "No" 1 "Yes"
lab val r13stroke_us stroke_lab
lab var r13stroke_us "Has a doctor ever diagnosed you with a stroke"
tab r13stroke_us
tab r13stroke_us,m
//5 missing values 

** TIME-SINCE-STROKE VARIABLE

//Categorizing by 10 years or less from HCAP (Wave 13 data)

////codebook states that "each of these variables is set to 'yes' if the Respondent answered yes to the pertinent question in the current or any prior wave, and to 'no' if the Respondent responded no at the current and all prior waves."

/*code for stroke_time variable (using wave 13 as stroke question (2016)
0= no stroke 
1= 1 year or less since stroke
2= 2 years since stroke
3= 3 years since stroke
4= 4 years since stroke 
5= 5-9 years since stroke 
6= 10 or more years since stroke 
//Cross-national study found that cogntiive decline was faster 1-3 years after stroke onset; SD would be 0.53 in global cognitive function over a decade after stroke.
//https://www.ahajournals.org/doi/full/10.1161/STROKEAHA.121.035796 Lo et al., 2021 
//
*/
tab r13agey_e r13recstrok if r13agey_e==65 //repeated this for all ages
tab age r13recstrok if age==76

gen stroke_time=.
replace stroke_time=0 if r13stroke==0 
replace stroke_time=1 if (r13recstrok==r13agey_e) | (r13agey_e== r13recstrok + 1) 
replace stroke_time=2 if (r13agey_e== r13recstrok + 2)
replace stroke_time=3 if (r13agey_e== r13recstrok + 3)
replace stroke_time=4 if (r13agey_e== r13recstrok + 4)
replace stroke_time=5 if (r13agey_e== r13recstrok + 5) | (r13agey_e== r13recstrok + 6) | (r13agey_e== r13recstrok + 7) | (r13agey_e== r13recstrok + 8) | (r13agey_e== r13recstrok + 9)
replace stroke_time=6 if (r13agey_e >= r13recstrok + 10) 
replace stroke_time=. if r13stroke==.d | r13stroke==.m  | r13stroke==. | (r13stroke==0 & r13recstrok <.)  
 
label define stroketime_lab 0 "No stroke" 1 "1 year or less" 2 "2 years" 3 "3 years" 4 "4 years" 5 "5-9 years" 6 "10 years or more"
label values stroke_time stroketime_lab
label variable stroke_time "Number of years since last stroke"

tab stroke_time
tab stroke_time r13stroke,m
*84 missing "yes" stroke values due to nonresponse to age at last stroke variable; dropped as missing because less than 5% of sample 
*25 individuals reported "no" to r13stroke but reported an age of their most recent stroke. These observations were dropped due to conflicting responses 


************************************
*************OUTCOME****************
************************************

mdesc fgcp fmem fexf forient flang 
//7 missing values for forient 


************************************
*************COVARIATES*************
************************************

******************************
****Demographic covariates****
******************************
*Continuous vars: age, wealth
mdesc age wealth4
summ age
tabstat wealth4, statistic(mean median iqr)
*education
tab educattain_resp_orig,m 
tab educattain_resp_orig,m nolab
gen us_educ= educattain_resp_orig
/*
code:
0= lower secondary education or below
1= upper secondary
2= higher than upper secondary 
*/
recode us_educ -1=0 1=0 2=0 3=1 5=2 6=2 7=2
tab us_educ,m 
label define edu_lab 0 "secondary or less" 1 "upper secondary" 2 "More than upper secondary"
label values us_educ edu_lab
label variable us_educ "Education level using ISCED education qualifications-USA"
tab us_educ

**Minority group status
tab mingrp,m 


*labor force status
tab r13lbrf,m
tab r13lbrf, nolab
gen r13lbrf_us= r13lbrf
recode r13lbrf_us 7=0 6=0 2=1 4=2 5=2 7=3 
label define labor_lab 0 "Not working or disabled" 1 "Working" 2 "Retired" 3 "Unemployed"
label values r13lbrf_us labor_lab
lab var r13lbrf_us "W13 Labor force status: HRS-HCAP"
tab r13lbrf_us r13lbrf,m

*marital status
tab r13mstat,m nolab
gen r13mstat_us=r13mstat
recode r13mstat_us .m=. 2=1 3=1 5=4
label define marital_lab 1 "married or living with partner" 4 "separated or divorced" 7 "widowed" 8 "never married"
label values r13mstat_us marital_lab
tab r13mstat_us


*******************************
********Health covariates******
*******************************

**Obese
//create separate wave obese var
gen obese12 =.
replace obese12=0 if (r12mbmi<30 & r12mbmi !=.) 
replace obese12=1 if (r12mbmi >30 & r12mbmi !=.)
replace obese12=2 if (r12mbmi==.d | r12mbmi==.n |r12mbmi==.s)
replace obese12=. if (r12mbmi==.m)
tab obese12,m

gen obese13=. 
replace obese13=0 if (r13mbmi<30 & r13mbmi !=.)
replace obese13=1 if (r13mbmi>30 & r13mbmi !=.)
replace obese13=2 if (r13mbmi==.n |r13mbmi==.s)
tab obese13,m

tab obese12 obese13,m
*should only have 51 missing values in final combined obese variable*

//create w12 obese variable*
*obese = >= 30
tab r13mbmi,m
tab r12mbmi,m


//combine w12 and w13 obese vars to create obese var, replace msising w13 with w12 data


//create obese variable so code means:
/*
0=Not obese 
1= Obese 
2= unknown both waves
*/
tab obese12 obese13,m
gen obese=.
replace obese=0 if (obese13==0) | (obese12==0 & obese13==2) 
replace obese=1 if (obese13==1) | (obese12==1 & obese13==2) 
replace obese=2 if (obese13==2 & obese12==2)
tab obese,m

**Drinking status

tab r13drink,m
tab r13drink,m nolab
recode r13drink .d=. .r=.
*recoded .d and .m and .r as missing since less than 5% of sample

tab r13drinkd,m
recode r13drinkd .d=. .r=.

tab r13drinkn,m
recode r13drinkn .d=. .r=.

//Create drinking status variable, categorizing based on CDC categories: https://www.cdc.gov/nchs/nhis/alcohol/alcohol_glossary.htm#:~:text=Current%20light%20drinker%20%E2%80%93%20At%20least,average%20over%20the%20past%20year.

table r13drink r13drinkd r13drinkn,m stat(freq)

/*
drinking status code:
0= Abstainer (does not drink any alcohol at all per week)
1= light drinker (3 drinks or fewer in a week)
2= moderate drinker (No more than 7 drinks per week for women and no more than 14 drinks per week for men)
3= Heavy drinker (More than 7 drinsk epr week for womena nd moer than 14 drinks per week for men)
4= drinks, undocumented how much 
*/
gen r13drinkstat=.
replace r13drinkstat=0 if (r13drink==0 & r13drinkn==0 & r13drinkd==0) 

replace r13drinkstat=1 if (r13drink==1 & r13drinkn==0 & r13drinkd==0) | (r13drink==1 & r13drinkd==1 & r13drinkn >=0 & r13drinkn <=3) | (r13drink==1 & r13drinkd==2 & r13drinkn==1) | (r13drink==1 & r13drinkd==3 & r13drinkn==1)

replace r13drinkstat=2 if (r13drink==1 & r13drinkd==1 & r13drinkn >3 & r13drinkn <=7) | (r13drink==1 & r13drinkd==1 & r13drinkn > 3 & r13drinkn <=14 & woman==0) | (r13drink==1 & r13drinkd==2 & r13drinkn>1 & r13drinkn <4) | (r13drink==1 & r13drinkd==2 & r13drinkn >=4 & woman==0) | (r13drink==1 & r13drinkd==3 & r13drinkn==2) | (r13drink==1 & r13drinkd==3 & r13drinkn>2 & r13drinkn<=4 & woman==0) | (r13drink==1 & r13drinkd==4 & r13drinkn==1) | (r13drink==1 & r13drinkd==4 & r13drinkn <4 & woman==0) | (r13drink==1 & r13drinkd==5 & r13drinkn==1) | (r13drink==1 & r13drinkd==5 & r13drinkn==2 & woman==0) | (r13drink==1 & r13drinkd==6 & r13drinkn==1) | (r13drink==1 & r13drinkd==6 & r13drinkn==2 & woman==0) | (r13drink==1 & r13drinkd==7 & r13drinkn==1) | (r13drink==1 & r13drinkd==7 & r13drinkn==2 & woman==0)
*

replace r13drinkstat=3 if (r13drink==1 & r13drinkd==1 & r13drinkn >7 & woman==1) | (r13drink==1 & r13drinkd==2 & r13drinkn >=4 & woman==1) | (r13drink==1 & r13drinkd==3 & r13drinkn >2 & woman==1) | (r13drink==1 & r13drinkd==3 & r13drinkn >= 5) | (r13drink==1 & r13drinkd==4 & r13drinkn >1 & woman==1) | (r13drink==1 & r13drinkd==4 & r13drinkn >3) | (r13drink==1 & r13drinkd==5 & r13drinkn==2 & woman==1) | (r13drink==1 & r13drinkd==5 & r13drinkn >2) | (r13drink==1 & r13drinkd==6 & r13drinkn==2 & woman==1)  | (r13drink==1 & r13drinkd==6 & r13drinkn>2) | (r13drink==1 & r13drinkd==7 & r13drinkn==2 & woman==1) | (r13drink==1 & r13drinkd==7 & r13drinkn>2)

replace r13drinkstat=4 if (r13drink==1 & r13drinkd==.) | (r13drink==1 & r13drinkn==.)
*

replace r13drinkstat=. if r13drink==.

label define drink_lab 0 "Abstain" 1 " Light drinker" 2 "Moderate drinker" 3 "Heavy drinker" 4 "Drinks, but unknown how much" 
label values r13drinkstat drink_lab
tab r13drinkstat r13drinkd,m
tab r13drinkstat r13drinkn,m
*good* 


**Smoking status

tab r13smoken,m
recode r13smoken .m=.
tab r13smokev,m
recode r13smokev .m=.
//recoded .m as missing since it means "other missing", and less than 5% of sample
tab r13smokev r13smoken,m

gen r13smokestat=.
replace r13smokestat=0 if (r13smoken==0 & r13smokev==0)
replace r13smokestat=1 if (r13smoken==0 & r13smokev==1)
replace r13smokestat=2 if (r13smoken==1 & r13smokev==1)
tab r13smokestat,m

label define smokestat_lab 0 "Never smoker" 1 "Former smoker" 2 "Current smoker"
label values r13smokestat smokestat_lab
tab r13smokestat

tab r13diabe,m
recode r13diabe .d=. .r=. .m=. 
tab r13diabe 
//recoded .d .r and .m as missing since less than 5% of sample 
tab r12diabe,m
recode r12diabe .d=. .r=. .m=. 
tab r12diabe,m
//recoded .d .r and .m as missing since less than 5% of sample 



tab r13hearte,m
recode r13hearte .d=. .m=.
tab r13hearte
//recoded .d and .m as missing since less than 5% of sample 
tab r12hearte,m
recode r12hearte .d=. .m=. .r=.
tab r12hearte,m


tab r13hibpe,m
recode r13hibpe .m=. .d=.
tab r13hibpe
//recoded .d and .m as missing since less than 5% of sample 
tab r13hibpe,m
recode r13hibpe .m=. .d=.
tab r13hibpe





*****************************************************************************************************
******************************** 4) Final analytic sample explanation************************************************
*****************************************************************************************************


**Eligiblility variable*
mdesc r13stroke forient fexf flang fgcp fmem age
gen eligible=. 
replace eligible=0 if (r13stroke ==. | forient ==. | age < 65 | age ==.)
replace eligible=1 if (r13stroke !=. & forient !=. & fexf !=. & flang !=. & fgcp !=. & fmem !=. & age >= 65 & age !=.)
tab eligible
*Inclusion: Non missing exposure & outcome data for wave 13; participated in wave 13 and HRS-HCAP

//SAMPLE SIZE: 3,340

// Boehme et al. stroke risk factors: age, sex, raceeth, hypertension, smoking, waist to hip ratio, diet, physical inactivity, hyperlipidemia, diabetes, alcohol consumption, cardiac causes, apolilipoprotein B to A1

//Livingston et al. dementia risk factors: age, raceeth (Mayeda et al., 2016), genetic makeup, alcohol, head injury, air polluation, education, hypertension, hearing impairment, smoking, obesity, depression, physical inactivity, diabetes, and infrequent social contact

//covariates to include based on livingston et al. (dementia risk factor article) and Boehme et al. (stroke risk factor article): age i.obese i.educattain_resp_new i.r13lbrf (labor force status in wave 13) wealth4 (household lifetime wealth) i.r13mstat (marital status at wave 13) i.r13hearte (hisotry of heart problems) i.r13diabe (history of diabetes) i.raracem (race/ethnicity) i.r13drink (drinking status) i.r13smokev (smoking status) 

** Save dataset for analysis

*save "/Users/beckymilan/Downloads/hrsstrokecog_analysis.dta", replace
