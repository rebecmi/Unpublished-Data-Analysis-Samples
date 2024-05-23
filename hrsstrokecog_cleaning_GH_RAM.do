/*
Project: DATA CLEANING: Cross-National Gender Differences in Post-Stroke Cognitive Function in the United States
Author: Rebecca A. Milan
Version: 05/15/2024
*/

** HRS/HRS-HCAP Stroke-Cog Data Cleaning Do-File **
* 1) Import HRS-HCAP mega-harmonized datasets
* 2) Merge with HRS-Longitudinal & HRS-RAND
* 3) Data cleaning/management
* 4) Final analytic sample explanation
   
*****************************************************************************************************   
******************************** 1) Import derived HRS-HCAP dataset from personal drive ********************************
*****************************************************************************************************


** merge harmonized HCAPs
use "HIDDEN", clear
merge 1:1 id using "HIDDEN", force
tab study


/*   


      study |      Freq.     Percent        Cum.
------------+-----------------------------------
          1 |      3,347        6.46        6.46
          2 |      1,273        2.46        8.92
          3 |      2,042        3.94       12.86
          4 |      1,777        3.43       16.29
          5 |        631        1.22       17.50
          6 |      2,319        4.48       21.98
          7 |      3,388        6.54       28.52
          9 |      9,755       18.82       47.34
         10 |     19,955       38.51       85.85
         11 |      7,333       14.15      100.00
------------+-----------------------------------
      Total |     51,820      100.00



*/
keep if _merge==3
drop _merge
*keep HRS-HCAP participants
keep if study == 1
tab study // N = 3,347 
destring id_hrs, replace
drop ragender  

*****************************************************************************************************
******************************** 2) Merge with HRS Longitudinal dataset and RAND HRS***************
*****************************************************************************************************

** merge harmonized HRS longitudinal file*
merge 1:1 id_hrs using "$HIDDEN", force
keep if _merge == 3
tab study //N= 3,347
drop _merge


** merge with RAND HRS dataset
merge 1:1 id_hrs using "$HIDDEN", force
keep if _merge == 3
tab study //N= 3,347

drop _merge



*****************************************************************************************************
******************************** 3)Data cleaning/Management*****************************************************
*****************************************************************************************************
//covariates to include based on livingston et al. (dementia risk factor article) and Boehme et al. (stroke risk factor article): age i.obese i.educattain_resp wealth4 i.mstat i.r13hearte i.r13diabe i.mingrp i.r13drink i.r13smokev 

/*
 Reference person did not respond to this wave
.D Don't know
.R Refused
.X Does not apply (specifics depends on variable)
.Q Data not available because the question was not asked
.U Reference person is not married (for spouse variables)
.V Spouse did not respond this wave (for spousal variables)
.S Information not available due to skip patterns, typically
because the interview is by proxy Respondent
.M Other missing
.N
a. No live interview (SwDDATEF, SwNDATEF)
b. No mars, likely nev mar (RwMNEV)
c. In NHM (R/SwHOMCAR)
d. No utilization (R/SwTOTMBF)
e. Not a pension, other (R/SwTYPF1-R/SwTYPF4)
f. No Plan (R/SwCOVRT)
g. None (R/SwHECOV1-R/SwHECOV3)
h. No respondent employer-p (R/SwHERTR1-R/SwHERTR3,
R/SwHERTS1-R/SwHERTS3, R/SwHERET)
i. Cog not asked (re-IW)/1 (R/SwMO, R/SwDY, R/SwYR,
R/SwDW, R/SwSCIS, R/SwCACT, R/SwPRES, R/SwVP, R/SwVOCAB)
j. No retirement plans (R/SwRETWSP, R/SwRETINC, R/SwRETLIV
k. Not retired (R/SwRETSAT, R/SwRYRCMP)
.Y No Exit Interview, No Death Recorded (for exit variables)
.Z No Exit Interview, Death Recorded (for exit variables)
*/

//RULE FOR MISSINGNESS: If less than 5% of sample is any of the missing value codes, recode as "."; if more than 5% is any of the missing codes, pull from previous wave and/or create a "unknown" category to represent unless missing code is "." or ".m" (recode as "." if ".m")


************************************
*************Effect Modifier********
************************************
** Gender
mdesc ragender 
tab ragender
gen woman = ragender 
recode woman 1=0 2=1
tab woman,m 
lab define woman_lab 0 "Man" 1 "Woman"
lab val woman woman_lab 
lab var woman "participant gender"
tab woman ragender,m

** Depression
tab r13depres
tab r13effort
tab r13sleepr
tab r13whappy //reverse
recode r13whappy (0 = 1) (1 = 0), gen(r13whappyrev)
tab r13whappyrev
tab r13flone
tab r13fsad
tab r13going
tab r13enlife //reverse
recode r13enlife (0 = 1) (1 = 0), gen(r13enliferev)
tab r13enliferev

** Continuous
egen r13cesd_tot = rowtotal(r13depres r13effort r13sleepr r13whappyrev r13flone r13fsad r13going r13enliferev)
tab r13cesd_tot

** Binary
gen r13cesd_bin=.
replace r13cesd_bin=0 if r13cesd_tot <= 3
replace r13cesd_bin=1 if r13cesd_tot > 3 & r13cesd_tot !=.
tab r13cesd_bin r13cesd_tot,m
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
label define labor_lab 0 "Not working or disabled" 1 "Working" 2 "Retired" 3 "Unemployed" 4 "Other"
label values r13lbrf_us labor_lab
lab var r13lbrf_us "W13 Labor force status: HRS-HCAP"
tab r13lbrf_us r13lbrf,m

*marital status
tab r13mstat,m nolab
gen r13mstat_us=r13mstat
recode r13mstat_us .m=. 2=0 3=0 1=0 4=1 5=1 7=2 8=3
label define marital_lab 0 "married or living with partner" 1 "separated or divorced" 2 "widowed" 3 "never married"
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
replace eligible=0 if (r13stroke_us ==. | forient ==. | age < 65 | age ==.)
replace eligible=1 if (r13stroke_us !=. & forient !=. & fexf !=. & flang !=. & fgcp !=. & fmem !=. & age >= 65 & age !=.)
tab eligible

gen eligible_2=.
replace eligible_2=0 if (cardio ==. | forient ==. | age < 65 | age ==.)
replace eligible_2=1 if (cardio !=. & forient !=. & fexf !=. & flang !=. & fgcp !=. & fmem !=. & age >= 65 & age !=.)
tab eligible_2

*Inclusion: Non missing exposure & outcome data for wave 13; participated in wave 13 and HRS-HCAP

//SAMPLE SIZE: 3,340 for stroke sample

** Create variables for merged country dataset
gen educ=us_educ
drop smokestat
gen smokestat=r13smokestat
gen drinkstat=r13drinkstat
gen hibp=r13hibpe
gen diab=r13diabe
gen heart=r13hearte
drop mstat
gen mstat=r13mstat_us
gen lbrf=r13lbrf_us
drop stroke
gen stroke=r13stroke_us


keep wgt fgcp fmem fexf flang forient cardio eligible stroke woman age obese lbrf wealth4 mstat heart diab hibp mingrp drinkstat smokestat educ study id
** Save dataset for analysis
save "HIDDEN", replace

