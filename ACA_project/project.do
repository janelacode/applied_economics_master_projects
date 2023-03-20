
log using template.log, replace
*========================================================================

*Project for  Applied Econometrics
*Sagynbaeva Zhanyl  -- UniNe

*========================================================================
**Initial commands to load  data
set more off, permanently
use full_dataJ.dta, clear
*========================================================================
**INITIAL DATA MANIPULATION 
*For this project we used  state-level annual cross sectional data from The Behavioural Risk Factor Surveillance System (BRFSS) and created our proper panel data. We kept as control and treatment group only those states which are mentioned in the articel "medicaid-expanion-explained)
*drop if _state==3 | _state==5 | _state==11 | _state==7 | _state==14 |_state==19 | _state==18 | _state==26 | _state==33 | _state==52 | _state== 66 | _state== 72 | _state == 78 | _state== 42

* Similarly, variables that had large share of missing values and that were not used in our models were removed as well. 
* I Will add more variables here (today)

destring iyear, replace 
xtset _state iyear
*========================================================================
*SUMMARY STATTISTICS 
xtsum menthlth physhlth poorhlth hlthpln1 medcost hispanc2 marital children educa employ1 income2 sex age_categories medicare
* interpretation 
*========================================================================
* Creating treatment, control groups. Also creating time dummies and post& pretreatement variables

*treated & control 
gen treated = 1 if _state==50|_state==25|_state==44|_state==9|_state==36|_state==34|_state==10|_state==24|_state==54|_state==51|_state==39|_state==21|_state==17|_state==27|_state==38|_state==8|_state==35|_state==4|_state==32|_state==6|_state==53|_state==41|_state==15
replace treated =0 if _state==16|_state==46|_state==55|_state==31|_state==20|_state==40|_state==48|_state==22|_state==1|_state==28|_state==12|_state==13|_state==45|_state==37|_state==23|_state==30|_state==56|_state==49|_state==47|_state==51|_state==29|_state==2

*post pre treatement variables
gen after = 1 if iyear== 2014|iyear== 2015| iyear== 2016| iyear== 2017
replace after =0 if iyear ==2011|iyear ==2012|iyear ==2013

*Interaction term 
gen treat_post = treated*after

* Year dummies 
tab iyear, gen (yr)


*========================================================================Parallel Trend
* We want that delta DD before treatment to be  zero. If they are not zero, that means there is a difference that changes  over time. And it will make a parallel trend to claim. It's never possible to give a post treatment parallel trend, but it's possible to give an evidence that before treatment, parallel trend holds in the data.
 
*Test for Parallel Trend Assumption on (poor menthal health)
bysort iyear: egen avg_menthlth_treat= mean(menthlth) if _state==50|_state==25|_state==44|_state==9|_state==36|_state==34|_state==10|_state==24|_state==54|_state==51|_state==39|_state==21|_state==17|_state==27|_state==38|_state==8|_state==35|_state==4|_state==32|_state==6|_state==53|_state==41|_state==15
bysort iyear: egen avg_menthlth_control= mean(menthlth)if _state==16|_state==46|_state==55|_state==31|_state==20|_state==40|_state==48|_state==22|_state==1|_stat ==28|_state==12|_state==13|_state==45|_state==37|_state==23|_state==30|_state==56|_state==49|_state==47|_state==51|_state==29|_state==2
twoway (line avg_menthlth_treat iyear, sort) (line avg_menthlth_control iyear, sort) (scatter menthlth iyear, sort), xtitle (Years) xline (4.5)

*Test for Parallel Trend Assumption on (poor menthal health) using regression


gen yr2011_treat = yr1*treated
gen yr2012_treat = yr2*treated
gen yr2013_treat = yr3*treated

xtreg menthlth treat_post  yr2011_treat yr2012_treat yr2-yr7 , fe cluster (_state)
xtreg menthlth treat_post yr2011_treat yr2012_treat yr2-yr7 sex educa marital children income2 medcost , fe cluster (_state)

coefplot, keep (yr2011_treat yr2012_treat) vertical
*Here we can do nice figure, if we can show 95CI for pre-treatment estimate (placebo estimate) , so if the  95% CI overlaps with zero ,  then we can claim that parallel trend holds before treatment and that it is likely to hold after treatment. 

*========================================================================
*Test for Parallel Trend Assumption on (poor physical health and mental health)
* poorhlth
bysort iyear: egen avg_poorhlth_treat= mean(poorhlth) if _state==50|_state==25|_state==44|_state==9|_state==36|_state==34|_state==10|_state==24|_state==54|_state==51|_state==39|_state==21|_state==17|_state==27|_state==38|_state==8|_state==35|_state==4|_state==32|_state==6|_state==53|_state==41|_state==15
bysort iyear: egen avg_poorhlth_control= mean(poorhlth)if _state==16|_state==46|_state==55|_state==31|_state==20|_state==40|_state==48|_state==22|_state==1|_stat ==28|_state==12|_state==13|_state==45|_state==37|_state==23|_state==30|_state==56|_state==49|_state==47|_state==51|_state==29|_state==2
twoway (line avg_poorhlth_treat iyear, sort) (line avg_poorhlth_control iyear, sort) (scatter poorhlth iyear, sort), xtitle (Years) xline (4.5)

*Test for Parallel Trend Assumption on (poor physical health) using regression
xtreg poorhlth treat_post yr2011_treat yr2012_treat yr2-yr7 , fe cluster (_state)
xtreg poorhlth treat_post  yr2011_treat yr2012_treat yr2-yr7 sex educa marital children income2 medcost , fe cluster (_state)
* statistically insignificant results ( assumption holds?)
coefplot, keep (yr2011_treat yr2012_treat) vertical

*========================================================================
*ESTIMaTE DDBETA: treatment effect on poor menthal health 
xtreg menthlth treated after treat_post yr2-yr7, fe vce(cluster _state)

xtreg menthlth treated after  treat_post  yr2-yr6 sex marital educa employ1 income2, fe cluster (_state)
*Checking how Beta DD changes overtime 

gen yr2015_treat= yr5*treated
gen yr2016_treat= yr6*treated
gen yr2017_treat= yr7* treated
xtreg menthlth  yr2015_treat-yr2017_treat   yr2-yr7, fe cluster(_state)


*========================================================================
*ESTIMATE DDBETA: treatment effect on poor physical or menthal health 
xtreg poorhlth  treated after treat_post  yr2-yr7, fe vce(cluster _state)

* now with controls, and  now with only controls that a reasonalbe but result is not significant (some controls were bad controls, so I didn't include them') 
xtreg poorhlth treated after treat_post  yr2-yr6 sex marital educa employ1 income2, fe cluster (_state)
*Checking how Beta DD changes overtime 



xtreg poorhlth yr2015_treat-yr2017_treat   yr2-yr7, fe cluster(_state)

*========================================================================

*========================================================================
log close 
