#########################################################################
################ Alternative Education effectiveness analysis
#########################################################################

# This piece of code analyses differences in later life outcomes between students who were referred to 
# Alternative Education (AE) between the ages of 13-16 and learners who were never referred to AE but
# who were matched with AE students using a propensity score matching model. 

# For details on the PSM model that produced the control group used here, see "PSM v[x].R" saved in the 
# same folder as this code. For details on the construction of the dataset used here, see "Alt Ed v[x].sql".


#########################################################################
################ Setup
#########################################################################

library(dplyr)
library(RODBC)
library(pROC)
library(ggplot2)
library(broom)
library(stringr)

setwd("~/Network-Shares/DataLabNas/MAA/MAA2021-60/Alternative_education")

# NOTE: Server name and port number in below string have been suppressed
con=odbcDriverConnect("Driver=ODBC Driver 17 for SQL Server; Trusted_Connection=YES; Server=[SERVER NAME],[PORT NUMBER];Database=IDI_Sandpit")

Followup <- sqlQuery(con,"select * from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_psm_followup]")

Followup$died[is.na(Followup$died)] <- 0 
Followup$in_nz[is.na(Followup$in_nz)] <- 0 

Followup$ncea_1[is.na(Followup$ncea_1)] <- 0 
Followup$ncea_2[is.na(Followup$ncea_2)] <- 0 
Followup$ncea_3[is.na(Followup$ncea_3)] <- 0 
Followup$ue[is.na(Followup$ue)] <- 0 
Followup$at_school[is.na(Followup$at_school)] <- 0 

Followup$tertiary_enrolment[is.na(Followup$tertiary_enrolment)] <- 0 
Followup$ito_enrolment[is.na(Followup$ito_enrolment)] <- 0 
Followup$any_tertiary_enrolment[is.na(Followup$any_tertiary_enrolment)] <- 0 
Followup$max_qual_tertiary[is.na(Followup$max_qual_tertiary)] <- 0 

Followup$income_wages[is.na(Followup$income_wages)] <- 0 
Followup$income_benefit[is.na(Followup$income_benefit)] <- 0 
Followup$income_total[is.na(Followup$income_total)] <- 0 

Followup$income_wages <- Followup$income_wages/1000 
Followup$income_benefit <- Followup$income_benefit/1000
Followup$income_total <- Followup$income_total/1000 

Followup$any_wages[is.na(Followup$any_wages)] <- 0 
Followup$any_benefit[is.na(Followup$any_benefit)] <- 0 

Followup$offender_any[is.na(Followup$offender_any)] <- 0 
Followup$offender_violent[is.na(Followup$offender_violent)] <- 0 
Followup$victim_any[is.na(Followup$victim_any)] <- 0 
Followup$victim_violent[is.na(Followup$victim_violent)] <- 0 
Followup$community_sentence[is.na(Followup$community_sentence)] <- 0 
Followup$custodial_sentence[is.na(Followup$custodial_sentence)] <- 0 

Followup$pho_enrolment[is.na(Followup$pho_enrolment)] <- 0 
Followup$ed_admissions_n[is.na(Followup$ed_admissions_n)] <- 0 
Followup$gp_contacts_n[is.na(Followup$gp_contacts_n)] <- 0 
Followup$ash_n[is.na(Followup$ash_n)] <- 0 

Followup$ln_income_wages <- log(Followup$income_wages+1)
Followup$ln_income_benefit <- log(Followup$income_benefit+1)
Followup$ln_income_total <- log(Followup$income_total+1)

Followup$propensity_bin <- ordered(Followup$propensity_bin, levels = c("phat lt 0.1"
                                                                       ,"phat 0.1_0.3"
                                                                       ,"phat 0.3_0.5"
                                                                       ,"phat 0.5_0.7"
                                                                       ,"phat gt 0.7"))

# Do the same for the attendance sensitivity dataset
Followup_attendance <- sqlQuery(con,"select * from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_psm_followup_attendance]")

Followup_attendance$died[is.na(Followup_attendance$died)] <- 0 
Followup_attendance$in_nz[is.na(Followup_attendance$in_nz)] <- 0 

Followup_attendance$ncea_1[is.na(Followup_attendance$ncea_1)] <- 0 
Followup_attendance$ncea_2[is.na(Followup_attendance$ncea_2)] <- 0 
Followup_attendance$ncea_3[is.na(Followup_attendance$ncea_3)] <- 0 
Followup_attendance$ue[is.na(Followup_attendance$ue)] <- 0 
Followup_attendance$at_school[is.na(Followup_attendance$at_school)] <- 0 

Followup_attendance$tertiary_enrolment[is.na(Followup_attendance$tertiary_enrolment)] <- 0 
Followup_attendance$ito_enrolment[is.na(Followup_attendance$ito_enrolment)] <- 0 
Followup_attendance$any_tertiary_enrolment[is.na(Followup_attendance$any_tertiary_enrolment)] <- 0 
Followup_attendance$max_qual_tertiary[is.na(Followup_attendance$max_qual_tertiary)] <- 0 

Followup_attendance$income_wages[is.na(Followup_attendance$income_wages)] <- 0 
Followup_attendance$income_benefit[is.na(Followup_attendance$income_benefit)] <- 0 
Followup_attendance$income_total[is.na(Followup_attendance$income_total)] <- 0 

Followup_attendance$income_wages <- Followup_attendance$income_wages/1000 
Followup_attendance$income_benefit <- Followup_attendance$income_benefit/1000
Followup_attendance$income_total <- Followup_attendance$income_total/1000 

Followup_attendance$any_wages[is.na(Followup_attendance$any_wages)] <- 0 
Followup_attendance$any_benefit[is.na(Followup_attendance$any_benefit)] <- 0 

Followup_attendance$offender_any[is.na(Followup_attendance$offender_any)] <- 0 
Followup_attendance$offender_violent[is.na(Followup_attendance$offender_violent)] <- 0 
Followup_attendance$victim_any[is.na(Followup_attendance$victim_any)] <- 0 
Followup_attendance$victim_violent[is.na(Followup_attendance$victim_violent)] <- 0 
Followup_attendance$community_sentence[is.na(Followup_attendance$community_sentence)] <- 0 
Followup_attendance$custodial_sentence[is.na(Followup_attendance$custodial_sentence)] <- 0 

Followup_attendance$pho_enrolment[is.na(Followup_attendance$pho_enrolment)] <- 0 
Followup_attendance$ed_admissions_n[is.na(Followup_attendance$ed_admissions_n)] <- 0 
Followup_attendance$gp_contacts_n[is.na(Followup_attendance$gp_contacts_n)] <- 0 
Followup_attendance$ash_n[is.na(Followup_attendance$ash_n)] <- 0 

Followup_attendance$ln_income_wages <- log(Followup_attendance$income_wages+1)
Followup_attendance$ln_income_benefit <- log(Followup_attendance$income_benefit+1)
Followup_attendance$ln_income_total <- log(Followup_attendance$income_total+1)

Followup_attendance$propensity_bin <- ordered(Followup_attendance$propensity_bin, levels = c("phat lt 0.1"
                                                                       ,"phat 0.1_0.3"
                                                                       ,"phat 0.3_0.5"
                                                                       ,"phat 0.5_0.7"
                                                                       ,"phat gt 0.7"))


# Do the same for the exact sensitivity dataset
Followup_exact <- sqlQuery(con,"select * from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_psm_followup_exact]")

Followup_exact$died[is.na(Followup_exact$died)] <- 0 
Followup_exact$in_nz[is.na(Followup_exact$in_nz)] <- 0 

Followup_exact$ncea_1[is.na(Followup_exact$ncea_1)] <- 0 
Followup_exact$ncea_2[is.na(Followup_exact$ncea_2)] <- 0 
Followup_exact$ncea_3[is.na(Followup_exact$ncea_3)] <- 0 
Followup_exact$ue[is.na(Followup_exact$ue)] <- 0 
Followup_exact$at_school[is.na(Followup_exact$at_school)] <- 0 

Followup_exact$tertiary_enrolment[is.na(Followup_exact$tertiary_enrolment)] <- 0 
Followup_exact$ito_enrolment[is.na(Followup_exact$ito_enrolment)] <- 0 
Followup_exact$any_tertiary_enrolment[is.na(Followup_exact$any_tertiary_enrolment)] <- 0 
Followup_exact$max_qual_tertiary[is.na(Followup_exact$max_qual_tertiary)] <- 0 

Followup_exact$income_wages[is.na(Followup_exact$income_wages)] <- 0 
Followup_exact$income_benefit[is.na(Followup_exact$income_benefit)] <- 0 
Followup_exact$income_total[is.na(Followup_exact$income_total)] <- 0 

Followup_exact$income_wages <- Followup_exact$income_wages/1000 
Followup_exact$income_benefit <- Followup_exact$income_benefit/1000
Followup_exact$income_total <- Followup_exact$income_total/1000 

Followup_exact$any_wages[is.na(Followup_exact$any_wages)] <- 0 
Followup_exact$any_benefit[is.na(Followup_exact$any_benefit)] <- 0 

Followup_exact$offender_any[is.na(Followup_exact$offender_any)] <- 0 
Followup_exact$offender_violent[is.na(Followup_exact$offender_violent)] <- 0 
Followup_exact$victim_any[is.na(Followup_exact$victim_any)] <- 0 
Followup_exact$victim_violent[is.na(Followup_exact$victim_violent)] <- 0 
Followup_exact$community_sentence[is.na(Followup_exact$community_sentence)] <- 0 
Followup_exact$custodial_sentence[is.na(Followup_exact$custodial_sentence)] <- 0 

Followup_exact$pho_enrolment[is.na(Followup_exact$pho_enrolment)] <- 0 
Followup_exact$ed_admissions_n[is.na(Followup_exact$ed_admissions_n)] <- 0 
Followup_exact$gp_contacts_n[is.na(Followup_exact$gp_contacts_n)] <- 0 
Followup_exact$ash_n[is.na(Followup_exact$ash_n)] <- 0 

Followup_exact$ln_income_wages <- log(Followup_exact$income_wages+1)
Followup_exact$ln_income_benefit <- log(Followup_exact$income_benefit+1)
Followup_exact$ln_income_total <- log(Followup_exact$income_total+1)

Followup_exact$propensity_bin <- ordered(Followup_exact$propensity_bin, levels = c("phat lt 0.1"
                                                                                             ,"phat 0.1_0.3"
                                                                                             ,"phat 0.3_0.5"
                                                                                             ,"phat 0.5_0.7"
                                                                                             ,"phat gt 0.7"))



#########################################################################
################ Main effects
#########################################################################

agelist <- c(17:30)

varlist <- c("died"
             ,"in_nz"
             ,"ncea_1"
             ,"ncea_2"
             ,"ncea_3"
             ,"ue"
             ,"at_school"
             ,"tertiary_enrolment"
             ,"ito_enrolment"
             ,"any_tertiary_enrolment"
             ,"max_qual_tertiary"
             ,"income_wages"
             ,"income_benefit"
             ,"income_total"
             ,"any_wages"
             ,"any_benefit"
             ,"ln_income_wages"
             ,"ln_income_benefit"
             ,"ln_income_total"
             ,"offender_any"
             ,"offender_violent"
             ,"victim_any"
             ,"victim_violent"
             ,"community_sentence"
             ,"custodial_sentence"
             ,"pho_enrolment"
             ,"ed_admissions_n"
             ,"gp_contacts_n"
             ,"ash_n"
             )


output <- data.frame(model=character(),
                     outcome=character(),
                     estimate=character(),
                     age=integer(),
                     coef=double(),
                     se=double(),
                     p=double(),
                     sum_treatment=integer(),
                     sum_control=integer(),
                     n_treatment=integer(),
                     schools_treatment=integer(),
                     schools_control=integer(),
                     terproviders_treatment=integer(),
                     terproviders_control=integer()
)


# Produce outcomes for preferred specification
for (a in agelist) {
for (v in varlist) {
tryCatch({
step_a <- lm(formula = eval(parse(text=v)) ~ alted_during 
             + male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
             + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
             + num_addresses_prior + mme_prior
             + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
             + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
             + id + adhd + asd + ors_prior
             + factor(birth_year)
             + mother_mh + father_mh + child_mha
             + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
             + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
             + teen_parent + mother_benefit_age12 + father_benefit_age12
             + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
             , data=subset(Followup,age==a))  

step_b <- tidy(step_a)

step_c <- subset(step_b,term=="alted_during")

step_d <- glance(step_a)

step_e_t <- subset(Followup, age==a & alted_during==1)

step_e_c <- subset(Followup, age==a & alted_during==0)

step_f <- data.frame(
  model = "Preferred",
  outcome = v,
  estimate = "Adjusted",
  age = a,
  coef = step_c$estimate,
  se = step_c$std.error,
  p = step_c$p.value,
  sum_treatment = NA,
  sum_control = NA,
  n_treatment = NA,
  schools_treatment = NA,
  schools_control = NA,
  terproviders_treatment = NA,
  terproviders_control = NA
)

output <- rbind(output,step_f)

step_g <- lm(formula = eval(parse(text=v)) ~ alted_during 
             , data=subset(Followup,age==a))

step_h <- tidy(step_g)

step_i <- glance(step_g)

step_j <- subset(step_h,term=="alted_during")

step_k <- data.frame(
  model = "Preferred",
  outcome = v,
  estimate = "Unadjusted",
  age = a,
  coef = NA,
  se = step_j$std.error,
  p = step_j$p.value,
  sum_treatment = sum(eval(parse(text=paste("step_e_t$",v,sep=""))), na.rm=TRUE),
  sum_control = sum(eval(parse(text=paste("step_e_c$",v,sep=""))), na.rm=TRUE),
  n_treatment = sum(!is.na(eval(parse(text=paste("step_e_t$",v,sep=""))))),
  schools_treatment = n_distinct(step_e_t$combined_providerid),
  schools_control = n_distinct(step_e_c$combined_providerid),
  terproviders_treatment = n_distinct(step_e_t$any_tertiary_providerid),
  terproviders_control = n_distinct(step_e_c$any_tertiary_providerid)
  )
  
output <- rbind(output,step_k)
print(paste("Completed: Preferred",v,a,sep=" "))
}, error=function(e){cat("ERROR:",conditionMessage(e),"\n")})
}}


# Produce outcomes for variant using exact matching
for (a in agelist) {
  for (v in varlist) {
    tryCatch({
    step_a <- lm(formula = eval(parse(text=v)) ~ alted_during 
                 + male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
                 + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                 + num_addresses_prior + mme_prior
                 + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                 + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                 + id + adhd + asd + ors_prior
                 + factor(birth_year)
                 + mother_mh + father_mh + child_mha
                 + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                 + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                 + teen_parent + mother_benefit_age12 + father_benefit_age12
                 + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                 , data=subset(Followup_exact,age==a))  
    
    step_b <- tidy(step_a)
    
    step_c <- subset(step_b,term=="alted_during")
    
    step_d <- glance(step_a)
    
    step_e_t <- subset(Followup_exact, age==a & alted_during==1)
    
    step_e_c <- subset(Followup_exact, age==a & alted_during==0)
    
    step_f <- data.frame(
      model = "Exact match",
      outcome = v,
      estimate = "Adjusted",
      age = a,
      coef = step_c$estimate,
      se = step_c$std.error,
      p = step_c$p.value,
      sum_treatment = NA,
      sum_control = NA,
      n_treatment = NA,
      schools_treatment = NA,
      schools_control = NA,
      terproviders_treatment = NA,
      terproviders_control = NA
      )
    
    output <- rbind(output,step_f)
    
    step_g <- lm(formula = eval(parse(text=v)) ~ alted_during 
                 , data=subset(Followup_exact,age==a))
    
    step_h <- tidy(step_g)
    
    step_i <- glance(step_g)
    
    step_j <- subset(step_h,term=="alted_during")
    
    step_k <- data.frame(
      model = "Exact match",
      outcome = v,
      estimate = "Unadjusted",
      age = a,
      coef = NA,
      se = step_j$std.error,
      p = step_j$p.value,
      sum_treatment = sum(eval(parse(text=paste("step_e_t$",v,sep=""))), na.rm=TRUE),
      sum_control = sum(eval(parse(text=paste("step_e_c$",v,sep=""))), na.rm=TRUE),
      n_treatment = sum(!is.na(eval(parse(text=paste("step_e_t$",v,sep=""))))),
      schools_treatment = n_distinct(step_e_t$combined_providerid),
      schools_control = n_distinct(step_e_c$combined_providerid),
      terproviders_treatment = n_distinct(step_e_t$any_tertiary_providerid),
      terproviders_control = n_distinct(step_e_c$any_tertiary_providerid)
      )
    
    output <- rbind(output,step_k)
    print(paste("Completed: Exact",v,a,sep=" "))
  }, error=function(e){cat("ERROR:",conditionMessage(e),"\n")})
  }}

# Produce outcomes for variant using attendance in the matching model (only for ages 17-22)
agelist <- c(17:22)

for (a in agelist) {
  for (v in varlist) {
    tryCatch({
    step_a <- lm(formula = eval(parse(text=v)) ~ alted_during 
                 + male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
                 + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                 + num_addresses_prior + mme_prior
                 + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                 + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                 + id + adhd + asd + ors_prior
                 + factor(birth_year)
                 + mother_mh + father_mh + child_mha
                 + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                 + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                 + teen_parent + mother_benefit_age12 + father_benefit_age12
                 + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                 , data=subset(Followup_attendance,age==a))  
    
    step_b <- tidy(step_a)
    
    step_c <- subset(step_b,term=="alted_during")
    
    step_d <- glance(step_a)
    
    step_e_t <- subset(Followup_attendance, age==a & alted_during==1)
    
    step_e_c <- subset(Followup_attendance, age==a & alted_during==0)
    
    step_f <- data.frame(
      model = "Attendance 1999-2004",
      outcome = v,
      estimate = "Adjusted",
      age = a,
      coef = step_c$estimate,
      se = step_c$std.error,
      p = step_c$p.value,
      sum_treatment = NA,
      sum_control = NA,
      n_treatment = NA,
      schools_treatment = NA,
      schools_control = NA,
      terproviders_treatment = NA,
      terproviders_control = NA
      )
    
    output <- rbind(output,step_f)
    
    step_g <- lm(formula = eval(parse(text=v)) ~ alted_during 
                 , data=subset(Followup_attendance,age==a))
    
    step_h <- tidy(step_g)
    
    step_i <- glance(step_g)
    
    step_j <- subset(step_h,term=="alted_during")
    
    step_k <- data.frame(
      model = "Attendance 1999-2004",
      outcome = v,
      estimate = "Unadjusted",
      age = a,
      coef = NA,
      se = step_j$std.error,
      p = step_j$p.value,
      sum_treatment = sum(eval(parse(text=paste("step_e_t$",v,sep=""))), na.rm=TRUE),
      sum_control = sum(eval(parse(text=paste("step_e_c$",v,sep=""))), na.rm=TRUE),
      n_treatment = sum(!is.na(eval(parse(text=paste("step_e_t$",v,sep=""))))),
      schools_treatment = n_distinct(step_e_t$combined_providerid),
      schools_control = n_distinct(step_e_c$combined_providerid),
      terproviders_treatment = n_distinct(step_e_t$any_tertiary_providerid),
      terproviders_control = n_distinct(step_e_c$any_tertiary_providerid)
      )
    
    output <- rbind(output,step_k)
    print(paste("Completed: Attendance 1999",v,a,sep=" "))
  }, error=function(e){cat("ERROR:",conditionMessage(e),"\n")})
  }}

# Produce comparable estimates for the same birth cohorts
for (a in agelist) {
  for (v in varlist) {
    tryCatch({
    step_a <- lm(formula = eval(parse(text=v)) ~ alted_during 
                 + male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
                 + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                 + num_addresses_prior + mme_prior
                 + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                 + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                 + id + adhd + asd + ors_prior
                 + factor(birth_year)
                 + mother_mh + father_mh + child_mha
                 + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                 + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                 + teen_parent + mother_benefit_age12 + father_benefit_age12
                 + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                 , data=subset(Followup,birth_year>=1999 & age==a))  
    
    step_b <- tidy(step_a)
    
    step_c <- subset(step_b,term=="alted_during")
    
    step_d <- glance(step_a)
    
    step_e_t <- subset(Followup, birth_year>=1999 & age==a & alted_during==1)
    
    step_e_c <- subset(Followup, birth_year>=1999 & age==a & alted_during==0)
    
    step_f <- data.frame(
      model = "Preferred 1999-2004",
      outcome = v,
      estimate = "Adjusted",
      age = a,
      coef = step_c$estimate,
      se = step_c$std.error,
      p = step_c$p.value,
      sum_treatment = NA,
      sum_control = NA,
      n_treatment = NA,
      schools_treatment = NA,
      schools_control = NA,
      terproviders_treatment = NA,
      terproviders_control = NA
      )
    
    output <- rbind(output,step_f)
    
    step_g <- lm(formula = eval(parse(text=v)) ~ alted_during 
                 , data=subset(Followup,birth_year>=1999 & age==a))
    
    step_h <- tidy(step_g)
    
    step_i <- glance(step_g)
    
    step_j <- subset(step_h,term=="alted_during")
    
    step_k <- data.frame(
      model = "Preferred 1999-2004",
      outcome = v,
      estimate = "Unadjusted",
      age = a,
      coef = NA,
      se = step_j$std.error,
      p = step_j$p.value,
      sum_treatment = sum(eval(parse(text=paste("step_e_t$",v,sep=""))), na.rm=TRUE),
      sum_control = sum(eval(parse(text=paste("step_e_c$",v,sep=""))), na.rm=TRUE),
      n_treatment = sum(!is.na(eval(parse(text=paste("step_e_t$",v,sep=""))))),
      schools_treatment = n_distinct(step_e_t$combined_providerid),
      schools_control = n_distinct(step_e_c$combined_providerid),
      terproviders_treatment = n_distinct(step_e_t$any_tertiary_providerid),
      terproviders_control = n_distinct(step_e_c$any_tertiary_providerid)
      )
    
    output <- rbind(output,step_k)
    print(paste("Completed: Preferred 1999",v,a,sep=" "))
  }, error=function(e){cat("ERROR:",conditionMessage(e),"\n")})
  }}

#########################################################################
################ Sensitivity analysis - quarter 1
#########################################################################

# Because we run our matching model using all data up until the end of the year before someone first gets referred to AE, any events that
# happen in the AE referring year (but before the precise referral date) are going to be missed by our model. This has the potential
# to introduce omitted variable bias. To test this, we create exactly the same estimates only for the learners who were referred to AE
# in January, February or March. The model is presumably most able to determine the current circumstances for these students.

Followup_q1 <- subset(Followup, qtr==1)

agelist <- c(17:30)

for (a in agelist) {
  for (v in varlist) {
    tryCatch({
      step_a <- lm(formula = eval(parse(text=v)) ~ alted_during 
                   + male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
                   + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                   + num_addresses_prior + mme_prior
                   + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                   + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                   + id + adhd + asd + ors_prior
                   + factor(birth_year)
                   + mother_mh + father_mh + child_mha
                   + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                   + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                   + teen_parent + mother_benefit_age12 + father_benefit_age12
                   + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                   , data=subset(Followup_q1,age==a))  
      
      step_b <- tidy(step_a)
      
      step_c <- subset(step_b,term=="alted_during")
      
      step_d <- glance(step_a)
      
      step_e_t <- subset(Followup_q1, age==a & alted_during==1)
      
      step_e_c <- subset(Followup_q1, age==a & alted_during==0)
      
      step_f <- data.frame(
        model = "Quarter 1 only",
        outcome = v,
        estimate = "Adjusted",
        age = a,
        coef = step_c$estimate,
        se = step_c$std.error,
        p = step_c$p.value,
        sum_treatment = NA,
        sum_control = NA,
        n_treatment = NA,
        schools_treatment = NA,
        schools_control = NA,
        terproviders_treatment = NA,
        terproviders_control = NA
      )
      
      output <- rbind(output,step_f)
      
      step_g <- lm(formula = eval(parse(text=v)) ~ alted_during 
                   , data=subset(Followup_q1,age==a))
      
      step_h <- tidy(step_g)
      
      step_i <- glance(step_g)
      
      step_j <- subset(step_h,term=="alted_during")
      
      step_k <- data.frame(
        model = "Quarter 1 only",
        outcome = v,
        estimate = "Unadjusted",
        age = a,
        coef = NA,
        se = step_j$std.error,
        p = step_j$p.value,
        sum_treatment = sum(eval(parse(text=paste("step_e_t$",v,sep=""))), na.rm=TRUE),
        sum_control = sum(eval(parse(text=paste("step_e_c$",v,sep=""))), na.rm=TRUE),
        n_treatment = sum(!is.na(eval(parse(text=paste("step_e_t$",v,sep=""))))),
        schools_treatment = n_distinct(step_e_t$combined_providerid),
        schools_control = n_distinct(step_e_c$combined_providerid),
        terproviders_treatment = n_distinct(step_e_t$any_tertiary_providerid),
        terproviders_control = n_distinct(step_e_c$any_tertiary_providerid)
      )
      
      output <- rbind(output,step_k)
      print(paste("Completed: Quarter 1",v,a,sep=" "))
    }, error=function(e){cat("ERROR:",conditionMessage(e),"\n")})
  }}



# Produce outcomes for variant using exact matching
Followup_exact_q1 <- subset(Followup_exact, qtr==1)

for (a in agelist) {
  for (v in varlist) {
    tryCatch({
      step_a <- lm(formula = eval(parse(text=v)) ~ alted_during 
                   + male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
                   + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                   + num_addresses_prior + mme_prior
                   + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                   + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                   + id + adhd + asd + ors_prior
                   + factor(birth_year)
                   + mother_mh + father_mh + child_mha
                   + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                   + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                   + teen_parent + mother_benefit_age12 + father_benefit_age12
                   + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                   , data=subset(Followup_exact_q1,age==a))  
      
      step_b <- tidy(step_a)
      
      step_c <- subset(step_b,term=="alted_during")
      
      step_d <- glance(step_a)
      
      step_e_t <- subset(Followup_exact_q1, age==a & alted_during==1)
      
      step_e_c <- subset(Followup_exact_q1, age==a & alted_during==0)
      
      step_f <- data.frame(
        model = "Exact match Q1 only",
        outcome = v,
        estimate = "Adjusted",
        age = a,
        coef = step_c$estimate,
        se = step_c$std.error,
        p = step_c$p.value,
        sum_treatment = NA,
        sum_control = NA,
        n_treatment = NA,
        schools_treatment = NA,
        schools_control = NA,
        terproviders_treatment = NA,
        terproviders_control = NA
      )
      
      output <- rbind(output,step_f)
      
      step_g <- lm(formula = eval(parse(text=v)) ~ alted_during 
                   , data=subset(Followup_exact_q1,age==a))
      
      step_h <- tidy(step_g)
      
      step_i <- glance(step_g)
      
      step_j <- subset(step_h,term=="alted_during")
      
      step_k <- data.frame(
        model = "Exact match Q1 only",
        outcome = v,
        estimate = "Unadjusted",
        age = a,
        coef = NA,
        se = step_j$std.error,
        p = step_j$p.value,
        sum_treatment = sum(eval(parse(text=paste("step_e_t$",v,sep=""))), na.rm=TRUE),
        sum_control = sum(eval(parse(text=paste("step_e_c$",v,sep=""))), na.rm=TRUE),
        n_treatment = sum(!is.na(eval(parse(text=paste("step_e_t$",v,sep=""))))),
        schools_treatment = n_distinct(step_e_t$combined_providerid),
        schools_control = n_distinct(step_e_c$combined_providerid),
        terproviders_treatment = n_distinct(step_e_t$any_tertiary_providerid),
        terproviders_control = n_distinct(step_e_c$any_tertiary_providerid)
      )
      
      output <- rbind(output,step_k)
      print(paste("Completed: Exact Q1",v,a,sep=" "))
    }, error=function(e){cat("ERROR:",conditionMessage(e),"\n")})
  }}

# Produce outcomes for variant using attendance in the matching model (only for ages 17-22)
Followup_attendance_q1 <- subset(Followup_attendance, qtr==1)

agelist <- c(17:22)

for (a in agelist) {
  for (v in varlist) {
    tryCatch({
      step_a <- lm(formula = eval(parse(text=v)) ~ alted_during 
                   + male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
                   + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                   + num_addresses_prior + mme_prior
                   + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                   + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                   + id + adhd + asd + ors_prior
                   + factor(birth_year)
                   + mother_mh + father_mh + child_mha
                   + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                   + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                   + teen_parent + mother_benefit_age12 + father_benefit_age12
                   + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                   , data=subset(Followup_attendance_q1,age==a))  
      
      step_b <- tidy(step_a)
      
      step_c <- subset(step_b,term=="alted_during")
      
      step_d <- glance(step_a)
      
      step_e_t <- subset(Followup_attendance_q1, age==a & alted_during==1)
      
      step_e_c <- subset(Followup_attendance_q1, age==a & alted_during==0)
      
      step_f <- data.frame(
        model = "Attendance 1999-2004 Q1 only",
        outcome = v,
        estimate = "Adjusted",
        age = a,
        coef = step_c$estimate,
        se = step_c$std.error,
        p = step_c$p.value,
        sum_treatment = NA,
        sum_control = NA,
        n_treatment = NA,
        schools_treatment = NA,
        schools_control = NA,
        terproviders_treatment = NA,
        terproviders_control = NA
      )
      
      output <- rbind(output,step_f)
      
      step_g <- lm(formula = eval(parse(text=v)) ~ alted_during 
                   , data=subset(Followup_attendance_q1,age==a))
      
      step_h <- tidy(step_g)
      
      step_i <- glance(step_g)
      
      step_j <- subset(step_h,term=="alted_during")
      
      step_k <- data.frame(
        model = "Attendance 1999-2004 Q1 only",
        outcome = v,
        estimate = "Unadjusted",
        age = a,
        coef = NA,
        se = step_j$std.error,
        p = step_j$p.value,
        sum_treatment = sum(eval(parse(text=paste("step_e_t$",v,sep=""))), na.rm=TRUE),
        sum_control = sum(eval(parse(text=paste("step_e_c$",v,sep=""))), na.rm=TRUE),
        n_treatment = sum(!is.na(eval(parse(text=paste("step_e_t$",v,sep=""))))),
        schools_treatment = n_distinct(step_e_t$combined_providerid),
        schools_control = n_distinct(step_e_c$combined_providerid),
        terproviders_treatment = n_distinct(step_e_t$any_tertiary_providerid),
        terproviders_control = n_distinct(step_e_c$any_tertiary_providerid)
      )
      
      output <- rbind(output,step_k)
      print(paste("Completed: Attendance 1999 Q1",v,a,sep=" "))
    }, error=function(e){cat("ERROR:",conditionMessage(e),"\n")})
  }}

# Produce comparable estimates for the same birth cohorts
for (a in agelist) {
  for (v in varlist) {
    tryCatch({
      step_a <- lm(formula = eval(parse(text=v)) ~ alted_during 
                   + male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
                   + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                   + num_addresses_prior + mme_prior
                   + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                   + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                   + id + adhd + asd + ors_prior
                   + factor(birth_year)
                   + mother_mh + father_mh + child_mha
                   + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                   + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                   + teen_parent + mother_benefit_age12 + father_benefit_age12
                   + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                   , data=subset(Followup_q1,birth_year>=1999 & age==a))  
      
      step_b <- tidy(step_a)
      
      step_c <- subset(step_b,term=="alted_during")
      
      step_d <- glance(step_a)
      
      step_e_t <- subset(Followup_q1, birth_year>=1999 & age==a & alted_during==1)
      
      step_e_c <- subset(Followup_q1, birth_year>=1999 & age==a & alted_during==0)
      
      step_f <- data.frame(
        model = "Preferred 1999-2004 Q1 only",
        outcome = v,
        estimate = "Adjusted",
        age = a,
        coef = step_c$estimate,
        se = step_c$std.error,
        p = step_c$p.value,
        sum_treatment = NA,
        sum_control = NA,
        n_treatment = NA,
        schools_treatment = NA,
        schools_control = NA,
        terproviders_treatment = NA,
        terproviders_control = NA
      )
      
      output <- rbind(output,step_f)
      
      step_g <- lm(formula = eval(parse(text=v)) ~ alted_during 
                   , data=subset(Followup_q1,birth_year>=1999 & age==a))
      
      step_h <- tidy(step_g)
      
      step_i <- glance(step_g)
      
      step_j <- subset(step_h,term=="alted_during")
      
      step_k <- data.frame(
        model = "Preferred 1999-2004 Q1 only",
        outcome = v,
        estimate = "Unadjusted",
        age = a,
        coef = NA,
        se = step_j$std.error,
        p = step_j$p.value,
        sum_treatment = sum(eval(parse(text=paste("step_e_t$",v,sep=""))), na.rm=TRUE),
        sum_control = sum(eval(parse(text=paste("step_e_c$",v,sep=""))), na.rm=TRUE),
        n_treatment = sum(!is.na(eval(parse(text=paste("step_e_t$",v,sep=""))))),
        schools_treatment = n_distinct(step_e_t$combined_providerid),
        schools_control = n_distinct(step_e_c$combined_providerid),
        terproviders_treatment = n_distinct(step_e_t$any_tertiary_providerid),
        terproviders_control = n_distinct(step_e_c$any_tertiary_providerid)
      )
      
      output <- rbind(output,step_k)
      print(paste("Completed: Preferred 1999 Q1",v,a,sep=" "))
    }, error=function(e){cat("ERROR:",conditionMessage(e),"\n")})
  }}












#########################################################################
################ Save all main effects to csv
#########################################################################

write.csv(output,file="output_effectiveness_analysis.csv", row.names=FALSE)



#########################################################################
################ Heterogeneity analysis
#########################################################################

# This breaks down the main effects by various factors (gender, ethnicity, region etc)

agelist <- c(17:30)
interactionlist <- c("eth_maori"
                     ,"eth_pacific"
                     ,"eth_european"
                     ,"eth_asian"
                     ,"sole_maori"
                     ,"male"
                     ,"factor(birth_year)"
                     ,"provider_at_15_region"
                     ,"propensity_bin"
                     ,"matching_model"
)

output_interactions <- data.frame(model=character(),
                                  outcome=character(),
                                  by=character(),
                                  term=character(),
                                  estimate=character(),
                                  age=integer(),
                                  coef=double(),
                                  se=double(),
                                  p=double(),
                                  n=double())


# Produce outcomes for preferred specification
for (i in interactionlist) {
  for (a in agelist) {
    for (v in varlist) {
      tryCatch({
        step_a <- lm(formula = eval(parse(text=v)) ~ eval(parse(text=i))*alted_during 
                     + male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
                     + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                     + num_addresses_prior + mme_prior
                     + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                     + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                     + id + adhd + asd + ors_prior
                     + factor(birth_year)
                     + mother_mh + father_mh + child_mha
                     + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                     + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                     + teen_parent + mother_benefit_age12 + father_benefit_age12
                     + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                     , data=subset(Followup,age==a))  
        
        step_b <- tidy(step_a)
        
        step_c <- subset(step_b,str_detect(term,"alted_during"))
        
        step_d <- glance(step_a)
        
        step_e <- data.frame(
          model = "Preferred",
          outcome = v,
          by = i,
          term = str_replace_all(step_c$term,"eval\\(parse\\(text = i\\)\\)",""),
          estimate = "Adjusted",
          age = a,
          coef = step_c$estimate,
          se = step_c$std.error,
          p = step_c$p.value,
          n = step_d$df + step_d$df.residual)
        
        output_interactions <- rbind(output_interactions,step_e)
        
        step_f <- lm(formula = eval(parse(text=v)) ~ eval(parse(text=i))*alted_during 
                     , data=subset(Followup,age==a))
        
        step_g <- tidy(step_f)
        
        step_h <- glance(step_f)
        
        step_i <- subset(step_g,str_detect(term,"alted_during"))
        
        step_j <- data.frame(
          model = "Preferred",
          outcome = v,
          by = i,
          term = str_replace_all(step_i$term,"eval\\(parse\\(text = i\\)\\)",""),
          estimate = "Unadjusted",
          age = a,
          coef = step_i$estimate,
          se = step_i$std.error,
          p = step_i$p.value,
          n = step_h$df + step_h$df.residual)
        
        output_interactions <- rbind(output_interactions,step_j)
        print(paste("Completed",i,v,a,sep=" "))
      }, error=function(e){cat("ERROR:",conditionMessage(e),"\n")})
    }}}

write.csv(output_interactions,file="output_heterogeneity_analysis.csv", row.names=FALSE)

