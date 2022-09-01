#age 14 sensitivity check analyses

#regression models
age14_nvq_unadjusted <- with(imputed_mcs2, lm(age14_standardised ~ highest_NVQ, weights = mcs2_weight))
age14_nvq_adjusted <- with(imputed_mcs2, lm(age14_standardised ~  gender +ethnicity +language_used_at_home +highest_NVQ, weights = mcs2_weight))
age14_income_unadjusted <- with(imputed_mcs2, lm(age14_standardised ~ income_quintiles, weights = mcs2_weight))
age14_income_adjusted <- with(imputed_mcs2, lm(age14_standardised ~ gender +ethnicity +language_used_at_home +income_quintiles, weights = mcs2_weight))
age14_wealth_unadjusted <- with(imputed_mcs2, lm(age14_standardised ~ wealth_quintiles, weights = mcs2_weight))
age14_wealth_adjusted <- with(imputed_mcs2, lm(age14_standardised ~  gender +ethnicity +language_used_at_home +wealth_quintiles, weights = mcs2_weight))
age14_occupation_unadjusted <- with(imputed_mcs2, lm(age14_standardised ~ occupational_status, weights = mcs2_weight))
age14_occupation_adjusted <- with(imputed_mcs2, lm(age14_standardised ~  gender +ethnicity +language_used_at_home+occupational_status, weights = mcs2_weight))
age14_imd_unadjusted <- with(imputed_mcs2, lm(age14_standardised ~ imd, weights = mcs2_weight))
age14_imd_adjusted <- with(imputed_mcs2, lm(age14_standardised ~  gender +ethnicity +language_used_at_home +imd, weights = mcs2_weight))

age14_confounders <- with(imputed_mcs2, lm(age14_standardised ~   gender +ethnicity +language_used_at_home , weights = mcs2_weight))

#results
round(summary(pool(age14_nvq_unadjusted), conf.int =TRUE),2)
round(summary(pool(age14_nvq_adjusted), conf.int =TRUE),2)
round(summary(pool(age14_income_unadjusted), conf.int =TRUE),2)
round(summary(pool(age14_income_adjusted), conf.int =TRUE),2)
round(summary(pool(age14_wealth_unadjusted), conf.int =TRUE),2)
round(summary(pool(age14_wealth_adjusted), conf.int =TRUE),2)
round(summary(pool(age14_occupation_unadjusted), conf.int =TRUE),2)
round(summary(pool(age14_occupation_adjusted), conf.int =TRUE),2)
round(summary(pool(age14_imd_unadjusted), conf.int =TRUE),2)
round(summary(pool(age14_imd_adjusted), conf.int =TRUE),2)

round(pool.r.squared(age14_nvq_adjusted),4)
round(pool.r.squared(age14_income_adjusted),4)
round(pool.r.squared(age14_wealth_adjusted),4)
round(pool.r.squared(age14_occupation_adjusted),4)
round(pool.r.squared(age14_imd_adjusted),4)
round(pool.r.squared(age14_confounders),4)
#export regression tables
#age14_nvq<-xtable(round(summary(pool(age14_nvq_adjusted), conf.int =TRUE),2))
#print.xtable(age14_nvq, type="html", file="mcs_age14_nvq.html")
#age14_income<-xtable(round(summary(pool(age14_income_adjusted), conf.int =TRUE),2))
#print.xtable(age14_income, type="html", file="mcs_age14_income.html")
#age14_wealth<-xtable(round(summary(pool(age14_wealth_adjusted), conf.int =TRUE),2))
#print.xtable(age14_wealth, type="html", file="mcs_age14_wealth.html")
#age14_occupation<-xtable(round(summary(pool(age14_occupation_adjusted), conf.int =TRUE),2))
#print.xtable(age14_occupation, type="html", file="mcs_age14_occupation.html")
#age14_imd<-xtable(round(summary(pool(age14_imd_adjusted), conf.int =TRUE),2))
#print.xtable(age14_imd, type="html", file="mcs_age14_imd.html")


#AIC values
get_stats_on_fit_from_MI(imputed_mcs2, "age14_standardised ~ gender +ethnicity +language_used_at_home +highest_NVQ, weights = mcs2_weight")
get_stats_on_fit_from_MI(imputed_mcs2, "age14_standardised ~ gender +ethnicity +language_used_at_home + income_quintiles, weights = mcs2_weight")
get_stats_on_fit_from_MI(imputed_mcs2, "age14_standardised ~ gender +ethnicity +language_used_at_home +wealth_quintiles, weights = mcs2_weight")
get_stats_on_fit_from_MI(imputed_mcs2, "age14_standardised ~ gender +ethnicity +language_used_at_home +occupational_status, weights = mcs2_weight")
get_stats_on_fit_from_MI(imputed_mcs2, "age14_standardised ~ gender +ethnicity +language_used_at_home +imd, weights = mcs2_weight")



#model with all SES predictors included 
age14_all_predictors<- with(imputed_mcs2, lm(age14_standardised ~   gender +ethnicity +language_used_at_home + highest_NVQ +income_quintiles+wealth_quintiles+occupational_status+imd, weights = mcs2_weight))
#model removing each predictor in turn 
age14_no_nvq<- with(imputed_mcs2, lm(age14_standardised ~   gender +ethnicity +language_used_at_home + income_quintiles+wealth_quintiles+occupational_status+imd, weights = mcs2_weight))
age14_no_income<- with(imputed_mcs2, lm(age14_standardised ~   gender +ethnicity +language_used_at_home + highest_NVQ +wealth_quintiles+occupational_status+imd, weights = mcs2_weight))
age14_no_wealth<- with(imputed_mcs2, lm(age14_standardised ~   gender +ethnicity +language_used_at_home + highest_NVQ +income_quintiles+occupational_status+imd, weights = mcs2_weight))
age14_no_occupation<- with(imputed_mcs2, lm(age14_standardised ~   gender +ethnicity +language_used_at_home + highest_NVQ +income_quintiles+wealth_quintiles+imd, weights = mcs2_weight))
age14_no_imd<- with(imputed_mcs2, lm(age14_standardised ~   gender +ethnicity +language_used_at_home + highest_NVQ +income_quintiles+wealth_quintiles+occupational_status, weights = mcs2_weight))
D1(age14_all_predictors, age14_no_nvq ,method = "wald")
D1(age14_all_predictors, age14_no_income ,method = "wald")
D1(age14_all_predictors, age14_no_wealth ,method = "wald")
D1(age14_all_predictors, age14_no_occupation ,method = "wald")
D1(age14_all_predictors, age14_no_imd ,method = "wald")

all_r2 = round(pool.r.squared(age14_all_predictors),4)*100

all_r2 - age14_confounders_r2
