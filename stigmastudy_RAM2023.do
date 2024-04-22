/*

Rebecca Milan's Semester Project
Stata Sample Code - No File Paths 

*/
**# Recode Maternal-Infant Bonding Scale variables based on negative outcome and positive outcome

gen neg_4 = MIBS4
recode neg_4 0=3 1=2 2=1 3=0
gen neg_3 = MIBS3
recode neg_3 0=3 1=2 2=1 3=0
gen neg_5=MIBS5
recode neg_5 0=3 1=2 2=1 3=0
gen neg_6=MIBS6 
recode neg_6 0=3 1=2 2=1 3=0 
gen neg_8= MIBS8
recode neg_8 0=3 1=2 2=1 3=0 

**# Replace all 88 and 99 as missing 
 
recode neg_5 88=.

**# Combine all MIBS vars into one (OUTCOME VARIABLE)

egen mibs_total = rowtotal(MIBS1 MIBS2 neg_3 neg_4 neg_5 neg_6 MIBS7 neg_8 MIBS9)

**# Combine LBWSS Internalized vars into 1 (1st predictor variable), and remove all 88 and 99 as missing

recode SBS7 99=. 88=.
recode SBS5 99=. 88=.
recode SBS3 99=. 88=.
recode SBS9 99=. 88=.
recode SBS8 99=. 88=.
recode SBS21 99=. 88=.
recode SBS2 99=. 88=.
recode SBSR1 99=. 88=.
egen SBS_I_total = rowtotal(SBS7 SBS5 SBS3 SBS9 SBS8 SBS21 SBS2 SBSR1)
gen sbs_i_total = SBS_I_total/8

**# Combine Low Birth Weight Stigma Scale Externalized vars into 1 (2nd predictor variable)

recode SBS16 99=. 88=.
recode SBS22 99=. 88=.
recode SBS17 99=. 88=.
recode SBS15R 99=. 88=.
recode SBS13 99=. 88=.
recode SBS14 99=. 88=.
recode SBS20 99=. 88=.
egen SBS_E_total = rowtotal(SBS16 SBS22 SBS17 SBS15R SBS13 SBS14 SBS20)
gen sbs_e_total = SBS_E_total/7

**# Combine PHQ9 questions to create depression variable 

egen depression = rowtotal(PHQ_9A PHQ_9B PHQ_9C PHQ_9D PHQ_9E PHQ_9F PHQ_9G PHQ_9H PHQ_9I)


**# Combine Perceived stress scale questions to create stress variable

egen stress = rowtotal(PSS1 PSS2 PSS3 PSS4)

**# Create age variable

gen age = Q44_1

**# Create work status variable

gen work_status = Q47
label define work_label 0 "do not work" 1 "work"
label values work_status work_label

**# Create education variable

gen education = Q50
label define education_label 0 "None" 1 "primary" 2 "Junior secondary" 3 "MSLC" 4 "secondary/technical" 5 "Tertiary and above"
label values education education_label

**# Create infant sex variable

gen baby_sex = Q12

**# Create religion variable 

gen religion = Q46

**# Create id variable

gen id = _n

**# Evaluate that assumptions are correct

*Linearity

binscatter mibs_total sbs_e_total

*Horizontal line (linearity is violated)

binscatter mibs_total sbs_i_total

*linearity not violated

twoway lfitci mibs_total sbs_e_total || scatter mibs_total sbs_e_total 

*horizontal line, not linear, constant

twoway lfitci mibs_total sbs_e_total || scatter mibs_total sbs_i_total 

*negative linear line

**#*Log transform externalizing stigma and check linearity assumption 

gen log_sbs_e = log(sbs_e_total)
binscatter mibs_total log_sbs_e

*it is now linear

*Normality

regress mibs_total sbs_i_total age i. baby_sex i. education i. work_status i.religion
predict predicted_I
predict residual_I, residual
predict rstandard_I, rstandard
stem rstandard_I

*residuals are normal

regress mibs_total log_sbs_e age i. baby_sex i. education i. work_status i.religion 
predict predicted_E
predict residual_E, residual
predict rstandard_E, rstandard
stem rstandard_E

*residuals are normal

*Equal variance

twoway (scatter residual_I predicted_I) || (lfit residual_I predicted_I)
twoway (scatter residual_E predicted_E) || (lfit residual_E predicted_E)

*equal variance assumption is correct

**# Multicollinearity test

regress mibs_total SBS_I_total age i. baby_sex i. education i. work_status i.religion
estat vif
regress mibs_total log_SBS_E age i. baby_sex i. education i. work_status i.religion
estat vif

*education may be an issue, but keep it because the covariates are already chosen in data description by Dr. Sakyi


**# Check for outliers and drop them if they exist

*Externalizing stigma outliers*
regress mibs_total log_sbs_e age i. baby_sex i. education i. work_status i.religion
dfbeta
list id residual_E rstandard_E if abs(_dfbeta_1)> 2/sqrt(145) &_dfbeta_1 <.

*76 and 95*

*Internalizing stigma outliers*

regress mibs_total sbs_i_total age i. baby_sex i. education i. work_status i.religion
dfbeta
list id residual_I rstandard_I if abs(_dfbeta_1)> 2/sqrt(145) &_dfbeta_1 <.

*74, 95*

**# Drop outliers 

*Internalizing data*

drop if id==74|id==95 
regress mibs_total sbs_i_total age i. baby_sex i. work_status i.religion

*Externalizing data*

drop if id==76||id==95
regress mibs_total log_sbs_e age i. baby_sex i. education i. work_status i.religion

*internalizing significant for all*

regress mibs_total sbs_i_total age i. baby_sex i. education i. work_status i.religion
regress mibs_total log_sbs_e age i. baby_sex i. education i. work_status i.religion


**#Multiple Linear regressions after dropping outliers

regress mibs_total sbs_i_total age i. baby_sex i. work_status i.religion i.education
regress mibs_total log_sbs_e age i. baby_sex i. work_status i.religion i.education


**#Determine if stress is a mediator for internalizing stigma using sem package*

sem(stress <- sbs_i_total age baby_sex education work_status religion)(mibs_total <- stress sbs_i_total age baby_sex education work_status religion)
estat teffects



**#Determine if depression is a mediator for internalizing using sem*

regress mibs_total sbs_i_total age i.baby_sex i.work_status i.religion i.education
sem(depression <- sbs_i_total age baby_sex work_status religion education)(mibs_total <- depression sbs_i_total age baby_sex work_status religion education)
estat teffects


**# Descriptive statistics of characteristics of mother-infant 

tab marital
tab work_status
tab education
tab Q49
tab religion
tab Q45
tab baby_sex
summarize age










