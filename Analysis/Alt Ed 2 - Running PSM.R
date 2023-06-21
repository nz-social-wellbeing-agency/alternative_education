#########################################################################
################ Alternative Education propensity score matching
#########################################################################

# Andrew Webber, 28/3/2023

# This piece of code constructs propensity score matching models that are used to pair students receiving
# Alternative Education (AE) with observably similar students not receiving AE. The code also produces the 
# coefficients of some logistic regression models that provide details on the sorts of students who receive AE.
# The code extracts data from sandpit tables that have been constructed for this purpose - for the code underlying 
# these tables, see "Alt Ed v[x].sql" saved in the same folder as this code.

# A separate matching model is constructed for each age - "_12" predicts the people referred to AE in the year they
# turn 13, based on only information up until the year they turn 12.
# Previous versions of this code also explored different ways of setting up the matching models, including one general
# model predicting all AE students at the same point (in the year they turn 12), as well as other models that subset
# the data in various ways. Those models were found to be less accurate than the age-specific models used here.

# This code saves the data for matched treatment/controls back in the sandpit, where the "Alt Ed v[x].sql" code
# picks up again, attaching subsequent life outcomes for ages 17-30. The code then transitions back to a separate 
# piece of R code that analyses the outcomes for the matched learners.


#########################################################################
################ Setup
#########################################################################

library(MatchIt)
library(dplyr)
library(RODBC)
library(pROC)
library(ggplot2)
library(broom)

setwd("~/Network-Shares/DataLabNas/MAA/MAA2021-60/Alternative_Education")

# NOTE: Server name and port number in below string have been suppressed
con=odbcDriverConnect("Driver=ODBC Driver 17 for SQL Server; Trusted_Connection=YES; Server=[SERVER NAME],[PORT NUMBER];Database=IDI_Sandpit")

# Note the age suffix refers to the age at the end of the year in which the matching is done 
# (ie _12 contains data up until the end of the year the learners turn 12, to predict who enters alt_ed in the year they turn 13)
AltEdPSM_12 <- sqlQuery(con,"select * from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_psm_age12_input]")
AltEdPSM_13 <- sqlQuery(con,"select * from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_psm_age13_input]")
AltEdPSM_14 <- sqlQuery(con,"select * from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_psm_age14_input]")
AltEdPSM_15 <- sqlQuery(con,"select * from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_psm_age15_input]")

# Variable creation

AltEdPSM_12[is.na(AltEdPSM_12)] <- 0 
AltEdPSM_13[is.na(AltEdPSM_13)] <- 0 
AltEdPSM_14[is.na(AltEdPSM_14)] <- 0 
AltEdPSM_15[is.na(AltEdPSM_15)] <- 0 

AltEdPSM_12$ln_motherinc_12 <- log(AltEdPSM_12$mother_totalinc_age12+1)
AltEdPSM_12$ln_fatherinc_12 <- log(AltEdPSM_12$mother_totalinc_age12+1)

AltEdPSM_13$ln_motherinc_13 <- log(AltEdPSM_13$mother_totalinc_age13+1)
AltEdPSM_13$ln_fatherinc_13 <- log(AltEdPSM_13$mother_totalinc_age13+1)

AltEdPSM_14$ln_motherinc_14 <- log(AltEdPSM_14$mother_totalinc_age14+1)
AltEdPSM_14$ln_fatherinc_14 <- log(AltEdPSM_14$mother_totalinc_age14+1)

AltEdPSM_15$ln_motherinc_15 <- log(AltEdPSM_15$mother_totalinc_age15+1)
AltEdPSM_15$ln_fatherinc_15 <- log(AltEdPSM_15$mother_totalinc_age15+1)

# Note: There are parts of this code where dataframes are saved back to the sandpit. This requires the Dataset Assembly Tool has been
# installed.


#########################################################################
################ Logits
#########################################################################

# Run logit regression (at age 12, predicting AE entry at 13)
logit_12 <- glm(alted_during ~ male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
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
             ,data=AltEdPSM_12, family="binomial")

prob_12 = predict(logit_12, type=c("response"))
AltEdPSM_12$prob = prob_12
roc_logit_12 <- roc(alted_during ~ prob, data=AltEdPSM_12)
png('graphs/auc_standard_12.png', width=800, height=800, res=300, pointsize=5)
plot(roc_logit_12, print.auc=TRUE)
dev.off()

output_logit_12 <- tidy(logit_12)
tempauc <- auc(roc_logit_12)
tempglance <- glance(logit_12)
tempdf <- data.frame(
  term = c("","n","auc"),
  estimate = c(NA,tempglance$df.null+tempglance$df.residual,round(tempauc,digits = 4))
)
output_logit_12 <- bind_rows(output_logit_12,tempdf)



# Run logit regression (at age 13, predicting AE entry at 14)
logit_13 <- glm(alted_during ~ male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
                + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                + num_addresses_prior + mme_prior
                + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                + id + adhd + asd + ors_prior
                + factor(birth_year)
                + mother_mh + father_mh + child_mha 
                + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                + teen_parent + mother_benefit_age13 + father_benefit_age13
                + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                ,data=AltEdPSM_13, family="binomial")

prob_13 = predict(logit_13, type=c("response"))
AltEdPSM_13$prob = prob_13
roc_logit_13 <- roc(alted_during ~ prob, data=AltEdPSM_13)
png('graphs/auc_standard_13.png', width=800, height=800, res=300, pointsize=5)
plot(roc_logit_13, print.auc=TRUE)
dev.off()

output_logit_13 <- tidy(logit_13)
tempauc <- auc(roc_logit_13)
tempglance <- glance(logit_13)
tempdf <- data.frame(
  term = c("","n","auc"),
  estimate = c(NA,tempglance$df.null+tempglance$df.residual,round(tempauc,digits = 4))
)
output_logit_13 <- bind_rows(output_logit_13,tempdf)


# Run logit regression (at age 14, predicting AE entry at 15)
logit_14 <- glm(alted_during ~ male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
                + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                + num_addresses_prior + mme_prior
                + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                + id + adhd + asd + ors_prior
                + factor(birth_year)
                + mother_mh + father_mh + child_mha 
                + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                + teen_parent + mother_benefit_age14 + father_benefit_age14
                + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                ,data=AltEdPSM_14, family="binomial")

prob_14 = predict(logit_14, type=c("response"))
AltEdPSM_14$prob = prob_14
roc_logit_14 <- roc(alted_during ~ prob, data=AltEdPSM_14)
png('graphs/auc_standard_14.png', width=800, height=800, res=300, pointsize=5)
plot(roc_logit_14, print.auc=TRUE)
dev.off()

output_logit_14 <- tidy(logit_14)
tempauc <- auc(roc_logit_14)
tempglance <- glance(logit_14)
tempdf <- data.frame(
  term = c("","n","auc"),
  estimate = c(NA,tempglance$df.null+tempglance$df.residual,round(tempauc,digits = 4))
)
output_logit_14 <- bind_rows(output_logit_14,tempdf)

# Run logit regression (at age 15, predicting AE entry at 16)
logit_15 <- glm(alted_during ~ male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
                + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                + num_addresses_prior + mme_prior
                + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                + id + adhd + asd + ors_prior
                + factor(birth_year)
                + mother_mh + father_mh + child_mha 
                + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                + teen_parent + mother_benefit_age15 + father_benefit_age15
                + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                ,data=AltEdPSM_15, family="binomial")

prob_15 = predict(logit_15, type=c("response"))
AltEdPSM_15$prob = prob_15
roc_logit_15 <- roc(alted_during ~ prob, data=AltEdPSM_15)
png('graphs/auc_standard_15.png', width=800, height=800, res=300, pointsize=5)
plot(roc_logit_15, print.auc=TRUE)
dev.off()


output_logit_15 <- tidy(logit_15)
tempauc <- auc(roc_logit_15)
tempglance <- glance(logit_15)
tempdf <- data.frame(
  term = c("","n","auc"),
  estimate = c(NA,tempglance$df.null+tempglance$df.residual,round(tempauc,digits = 4))
)
output_logit_15 <- bind_rows(output_logit_15,tempdf)

# Remove logit models and ROCs from data environment
rm(logit_12)
rm(logit_13)
rm(logit_14)
rm(logit_15)
rm(roc_logit_12)
rm(roc_logit_13)
rm(roc_logit_14)
rm(roc_logit_15)

# Save regressions to csv
write.csv(output_logit_12,file="output_logit_age12.csv", row.names=FALSE)
write.csv(output_logit_13,file="output_logit_age13.csv", row.names=FALSE)
write.csv(output_logit_14,file="output_logit_age14.csv", row.names=FALSE)
write.csv(output_logit_15,file="output_logit_age15.csv", row.names=FALSE)


#########################################################################
################ Matching
#########################################################################

# Undertake matching at age 12 (matching the age 13 entrants)
m.out_12 = matchit(formula = alted_during ~ male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
                + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                + num_addresses_prior + mme_prior
                + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                + id + adhd + asd + ors_prior
                + mother_mh + father_mh + child_mha 
                + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                + teen_parent + mother_benefit_age12 + father_benefit_age12
                + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                ,data=AltEdPSM_12, exact=(~ birth_year), method="nearest")
summary(m.out_12, standardize=TRUE)

psm_output_12 <- data.frame(match.data(m.out_12))

# Undertake matching at age 13 (matching the age 14 entrants)
m.out_13 = matchit(formula = alted_during ~ male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
                   + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                   + num_addresses_prior + mme_prior
                   + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                   + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                   + id + adhd + asd + ors_prior
                   + mother_mh + father_mh + child_mha 
                   + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                   + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                   + teen_parent + mother_benefit_age13 + father_benefit_age13
                   + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                   ,data=AltEdPSM_13, exact=(~ birth_year), method="nearest")
summary(m.out_13, standardize=TRUE)

psm_output_13 <- data.frame(match.data(m.out_13))

# Undertake matching at age 14 (matching the age 15 entrants)
m.out_14 = matchit(formula = alted_during ~ male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
                   + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                   + num_addresses_prior + mme_prior
                   + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                   + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                   + id + adhd + asd + ors_prior
                   + mother_mh + father_mh + child_mha 
                   + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                   + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                   + teen_parent + mother_benefit_age14 + father_benefit_age14
                   + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                   ,data=AltEdPSM_14, exact=(~ birth_year), method="nearest")
summary(m.out_14, standardize=TRUE)

psm_output_14 <- data.frame(match.data(m.out_14))

# Undertake matching at age 15 (matching the age 16 entrants)
m.out_15 = matchit(formula = alted_during ~ male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
                   + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                   + num_addresses_prior + mme_prior
                   + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                   + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                   + id + adhd + asd + ors_prior
                   + mother_mh + father_mh + child_mha
                   + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                   + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                   + teen_parent + mother_benefit_age15 + father_benefit_age15
                   + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                   ,data=AltEdPSM_15, exact=(~ birth_year), method="nearest")
summary(m.out_15, standardize=TRUE)

psm_output_15 <- data.frame(match.data(m.out_15))

# Save to sandpit
source("utility_functions.R")
source("dbplyr_helper_functions.R")
db_con = create_database_connection(database = "IDI_Sandpit")

# Saving preferred matching data
new_table = copy_r_to_sql(
  db_connection = db_con,
  db = "[IDI_Sandpit]",
  schema = "[DL-MAA2021-60]",
  sql_table_name = "[ae_202210_psm_age12_output]",
  r_table_name = psm_output_12,
  OVERWRITE = TRUE
)

new_table = copy_r_to_sql(
  db_connection = db_con,
  db = "[IDI_Sandpit]",
  schema = "[DL-MAA2021-60]",
  sql_table_name = "[ae_202210_psm_age13_output]",
  r_table_name = psm_output_13,
  OVERWRITE = TRUE
)

new_table = copy_r_to_sql(
  db_connection = db_con,
  db = "[IDI_Sandpit]",
  schema = "[DL-MAA2021-60]",
  sql_table_name = "[ae_202210_psm_age14_output]",
  r_table_name = psm_output_14,
  OVERWRITE = TRUE
)

new_table = copy_r_to_sql(
  db_connection = db_con,
  db = "[IDI_Sandpit]",
  schema = "[DL-MAA2021-60]",
  sql_table_name = "[ae_202210_psm_age15_output]",
  r_table_name = psm_output_15,
  OVERWRITE = TRUE
)

close_database_connection(db_con)

# Remove matching lists (not the resulting dataframes)
rm(m.out_12)
rm(m.out_13)
rm(m.out_14)
rm(m.out_15)


# Distribution of propensities
ggplot(psm_output_12, aes(x = distance, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) + 
  labs(fill='AE referral', x='Predicted probability (age 12)', y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/propensity_standard_12.png', width = 6, height = 4, units='in', dpi = 300)

ggplot(psm_output_13, aes(x = distance, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) + 
  labs(fill='AE referral', x='Predicted probability (age 13)', y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/propensity_standard_13.png', width = 6, height = 4, units='in', dpi = 300)

ggplot(psm_output_14, aes(x = distance, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) + 
  labs(fill='AE referral', x='Predicted probability (age 14)', y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/propensity_standard_14.png', width = 6, height = 4, units='in', dpi = 300)

ggplot(psm_output_15, aes(x = distance, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) + 
  labs(fill='AE referral', x='Predicted probability (age 15)', y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/propensity_standard_15.png', width = 6, height = 4, units='in', dpi = 300)


# Distribution of income
ggplot(subset(psm_output_12), aes(x = ln_motherinc_12, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) +
  labs(fill='AE referral', x="Mother's income at age 12 (log)", y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/mincome_standard_12.png', width = 6, height = 4, units='in', dpi = 300)

ggplot(subset(psm_output_12), aes(x = ln_fatherinc_12, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) +
  labs(fill='AE referral', x="Father's income at age 12 (log)", y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/fincome_standard_12.png', width = 6, height = 4, units='in', dpi = 300)

ggplot(subset(psm_output_13), aes(x = ln_motherinc_13, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) +
  labs(fill='AE referral', x="Mother's income at age 13 (log)", y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/mincome_standard_13.png', width = 6, height = 4, units='in', dpi = 300)

ggplot(subset(psm_output_13), aes(x = ln_fatherinc_13, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) +
  labs(fill='AE referral', x="Father's income at age 13 (log)", y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/fincome_standard_13.png', width = 6, height = 4, units='in', dpi = 300)

ggplot(subset(psm_output_14), aes(x = ln_motherinc_14, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) +
  labs(fill='AE referral', x="Mother's income at age 14 (log)", y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/mincome_standard_14.png', width = 6, height = 4, units='in', dpi = 300)

ggplot(subset(psm_output_14), aes(x = ln_fatherinc_14, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) +
  labs(fill='AE referral', x="Father's income at age 14 (log)", y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/fincome_standard_14.png', width = 6, height = 4, units='in', dpi = 300)

ggplot(subset(psm_output_15), aes(x = ln_motherinc_15, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) +
  labs(fill='AE referral', x="Mother's income at age 15 (log)", y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/mincome_standard_15.png', width = 6, height = 4, units='in', dpi = 300)

ggplot(subset(psm_output_15), aes(x = ln_fatherinc_15, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) +
  labs(fill='AE referral', x="Father's income at age 15 (log)", y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/fincome_standard_15.png', width = 6, height = 4, units='in', dpi = 300)


# Distribution of attendance (for those with attendance data)
## This is checking to see whether we are balancing on unobservables (assuming attendance is a relevant factor for subsequent AE referral)
ggplot(subset(psm_output_12,missing_attendance==0), aes(x = attendance_12_p, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) +
  labs(fill='AE referral', x="Attendance rate at age 12", y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/attendp_standard_12.png', width = 6, height = 4, units='in', dpi = 300)

ggplot(subset(psm_output_12,missing_attendance==0), aes(x = attendance_12_u, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) +
  labs(fill='AE referral', x="Unjustified absence rate at age 12", y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/attendu_standard_12.png', width = 6, height = 4, units='in', dpi = 300)

ggplot(subset(psm_output_13,missing_attendance==0), aes(x = attendance_13_p, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) +
  labs(fill='AE referral', x="Attendance rate at age 13", y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/attendp_standard_13.png', width = 6, height = 4, units='in', dpi = 300)

ggplot(subset(psm_output_13,missing_attendance==0), aes(x = attendance_13_u, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) +
  labs(fill='AE referral', x="Unjustified absence rate at age 13", y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/attendu_standard_13.png', width = 6, height = 4, units='in', dpi = 300)

ggplot(subset(psm_output_14,missing_attendance==0), aes(x = attendance_14_p, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) +
  labs(fill='AE referral', x="Attendance rate at age 14", y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/attendp_standard_14.png', width = 6, height = 4, units='in', dpi = 300)

ggplot(subset(psm_output_14,missing_attendance==0), aes(x = attendance_14_u, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) +
  labs(fill='AE referral', x="Unjustified absence rate at age 14", y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/attendu_standard_14.png', width = 6, height = 4, units='in', dpi = 300)

ggplot(subset(psm_output_15,missing_attendance==0), aes(x = attendance_15_p, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) +
  labs(fill='AE referral', x="Attendance rate at age 15", y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/attendp_standard_15.png', width = 6, height = 4, units='in', dpi = 300)

ggplot(subset(psm_output_15,missing_attendance==0), aes(x = attendance_15_u, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) +
  labs(fill='AE referral', x="Unjustified absence rate at age 15", y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/attendu_standard_15.png', width = 6, height = 4, units='in', dpi = 300)


#########################################################################
################ Sensitivity analysis - using attendance
#########################################################################

# The previous analysis indicated that the distribution of prior attendance rates is noticeably different between the treatment and matched
# controls. Non-attendance is conceptually highly linked to AE referral, so there are two concerns here:
#   a. That the omission of attendance in the matching model is creating a biased control group
#   b. That attendance is standing in for other important factors we can't observe
# We did not include attendance in the previous matching models because it is available for only some of the birth cohorts 
# (born since 1996-1999, depending on age at match). Therefore, we can use this as a test of robustness for our preferred 
# matching model: we run a separate matching process for those born 1999-2005 which INCLUDES attendance, and then conduct 
# followup analysis for the: 
#   a. treatment group,
#   b. control group (matched without attendance), and 
#   c. control group (matched with attendance)
# then differences between groups b and c (in terms of later life outcomes) will tell us to what extent our ultimate findings 
# are sensitive to the omission of attendance data (and potentially other omitted variables more broadly).
# As before, we will first run logits to recover coefficients, and then run the full matching process.

AltEdPSM_12_attendance <- subset(AltEdPSM_12, birth_year>=1999)
AltEdPSM_13_attendance <- subset(AltEdPSM_13, birth_year>=1999)
AltEdPSM_14_attendance <- subset(AltEdPSM_14, birth_year>=1999)
AltEdPSM_15_attendance <- subset(AltEdPSM_15, birth_year>=1999)

# Run logit regression (at age 12, predicting AE entry at 13)
logit_12_attendance <- glm(alted_during ~ male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
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
                + attendance_12_u + attendance_12_j + missing_attendance
                ,data=AltEdPSM_12_attendance, family="binomial")

prob_12_attendance = predict(logit_12_attendance, type=c("response"))
AltEdPSM_12_attendance$prob_12_attendance = prob_12_attendance
roc_logit_12_attendance <- roc(alted_during ~ prob_12_attendance, data=AltEdPSM_12_attendance)
png('graphs/auc_attend_12.png', width=800, height=800, res=300, pointsize=5)
plot(roc_logit_12_attendance, print.auc=TRUE)
dev.off()

output_logit_12_attendance <- tidy(logit_12_attendance)
tempauc <- auc(roc_logit_12_attendance)
tempglance <- glance(logit_12_attendance)
tempdf <- data.frame(
  term = c("","n","auc"),
  estimate = c(NA,tempglance$df.null+tempglance$df.residual,round(tempauc,digits = 4))
)
output_logit_12_attendance <- bind_rows(output_logit_12_attendance,tempdf)

# Run logit regression (at age 13, predicting AE entry at 14)
logit_13_attendance <- glm(alted_during ~ male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
                + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                + num_addresses_prior + mme_prior
                + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                + id + adhd + asd + ors_prior
                + factor(birth_year)
                + mother_mh + father_mh + child_mha
                + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                + teen_parent + mother_benefit_age13 + father_benefit_age13
                + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                + attendance_13_u + attendance_13_j + missing_attendance
                ,data=AltEdPSM_13_attendance, family="binomial")

prob_13_attendance = predict(logit_13_attendance, type=c("response"))
AltEdPSM_13_attendance$prob_13_attendance = prob_13_attendance
roc_logit_13_attendance <- roc(alted_during ~ prob_13_attendance, data=AltEdPSM_13_attendance)
png('graphs/auc_attend_13.png', width=800, height=800, res=300, pointsize=5)
plot(roc_logit_13_attendance, print.auc=TRUE)
dev.off()

output_logit_13_attendance <- tidy(logit_13_attendance)
tempauc <- auc(roc_logit_13_attendance)
tempglance <- glance(logit_13_attendance)
tempdf <- data.frame(
  term = c("","n","auc"),
  estimate = c(NA,tempglance$df.null+tempglance$df.residual,round(tempauc,digits = 4))
)
output_logit_13_attendance <- bind_rows(output_logit_13_attendance,tempdf)

# Run logit regression (at age 14, predicting AE entry at 15)
logit_14_attendance <- glm(alted_during ~ male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
                + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                + num_addresses_prior + mme_prior
                + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                + id + adhd + asd + ors_prior
                + factor(birth_year)
                + mother_mh + father_mh + child_mha
                + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                + teen_parent + mother_benefit_age14 + father_benefit_age14
                + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                + attendance_14_u + attendance_14_j + missing_attendance
                ,data=AltEdPSM_14_attendance, family="binomial")

prob_14_attendance = predict(logit_14_attendance, type=c("response"))
AltEdPSM_14_attendance$prob_14_attendance = prob_14_attendance
roc_logit_14_attendance <- roc(alted_during ~ prob_14_attendance, data=AltEdPSM_14_attendance)
png('graphs/auc_attend_14.png', width=800, height=800, res=300, pointsize=5)
plot(roc_logit_14_attendance, print.auc=TRUE)
dev.off()

output_logit_14_attendance <- tidy(logit_14_attendance)
tempauc <- auc(roc_logit_14_attendance)
tempglance <- glance(logit_14_attendance)
tempdf <- data.frame(
  term = c("","n","auc"),
  estimate = c(NA,tempglance$df.null+tempglance$df.residual,round(tempauc,digits = 4))
)
output_logit_14_attendance <- bind_rows(output_logit_14_attendance,tempdf)

# Run logit regression (at age 15, predicting AE entry at 16)
logit_15_attendance <- glm(alted_during ~ male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
                + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                + num_addresses_prior + mme_prior
                + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                + id + adhd + asd + ors_prior
                + factor(birth_year)
                + mother_mh + father_mh + child_mha
                + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                + teen_parent + mother_benefit_age15 + father_benefit_age15
                + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                + attendance_15_u + attendance_15_j + missing_attendance
                ,data=AltEdPSM_15_attendance, family="binomial")

prob_15_attendance = predict(logit_15_attendance, type=c("response"))
AltEdPSM_15_attendance$prob_15_attendance = prob_15_attendance
roc_logit_15_attendance <- roc(alted_during ~ prob_15_attendance, data=AltEdPSM_15_attendance)
png('graphs/auc_attend_15.png', width=800, height=800, res=300, pointsize=5)
plot(roc_logit_15_attendance, print.auc=TRUE)
dev.off()

output_logit_15_attendance <- tidy(logit_15_attendance)
tempauc <- auc(roc_logit_15_attendance)
tempglance <- glance(logit_15_attendance)
tempdf <- data.frame(
  term = c("","n","auc"),
  estimate = c(NA,tempglance$df.null+tempglance$df.residual,round(tempauc,digits = 4))
)
output_logit_15_attendance <- bind_rows(output_logit_15_attendance,tempdf)

# Save regressions to csv
write.csv(output_logit_12_attendance,file="output_logit_age12_attendance.csv", row.names=FALSE)
write.csv(output_logit_13_attendance,file="output_logit_age13_attendance.csv", row.names=FALSE)
write.csv(output_logit_14_attendance,file="output_logit_age14_attendance.csv", row.names=FALSE)
write.csv(output_logit_15_attendance,file="output_logit_age15_attendance.csv", row.names=FALSE)

# Remove logit models and ROCs from data environment
rm(logit_12_attendance)
rm(logit_13_attendance)
rm(logit_14_attendance)
rm(logit_15_attendance)
rm(roc_logit_12_attendance)
rm(roc_logit_13_attendance)
rm(roc_logit_14_attendance)
rm(roc_logit_15_attendance)

#########################################################################
################ Matching using attendance data
#########################################################################

# Undertake matching at age 12 (matching the age 13 entrants)
m.out_12_attendance = matchit(formula = alted_during ~ male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
                   + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                   + num_addresses_prior + mme_prior
                   + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                   + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                   + id + adhd + asd + ors_prior
                   + mother_mh + father_mh + child_mha 
                   + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                   + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                   + teen_parent + mother_benefit_age12 + father_benefit_age12
                   + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                   + attendance_12_u + attendance_12_j + missing_attendance
                   ,data=AltEdPSM_12_attendance, exact=(~ birth_year), method="nearest")
summary(m.out_12_attendance, standardize=TRUE)

psm_output_12_attendance <- data.frame(match.data(m.out_12_attendance))

# Undertake matching at age 13 (matching the age 14 entrants)
m.out_13_attendance = matchit(formula = alted_during ~ male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
                   + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                   + num_addresses_prior + mme_prior
                   + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                   + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                   + id + adhd + asd + ors_prior
                   + mother_mh + father_mh + child_mha 
                   + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                   + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                   + teen_parent + mother_benefit_age13 + father_benefit_age13
                   + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                   + attendance_13_u + attendance_13_j + missing_attendance
                   ,data=AltEdPSM_13_attendance, exact=(~ birth_year), method="nearest")
summary(m.out_13_attendance, standardize=TRUE)

psm_output_13_attendance <- data.frame(match.data(m.out_13_attendance))

# Undertake matching at age 14 (matching the age 15 entrants)
m.out_14_attendance = matchit(formula = alted_during ~ male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
                   + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                   + num_addresses_prior + mme_prior
                   + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                   + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                   + id + adhd + asd + ors_prior
                   + mother_mh + father_mh + child_mha 
                   + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                   + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                   + teen_parent + mother_benefit_age14 + father_benefit_age14
                   + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                   + attendance_14_u + attendance_14_j + missing_attendance
                   ,data=AltEdPSM_14_attendance, exact=(~ birth_year), method="nearest")
summary(m.out_14_attendance, standardize=TRUE)

psm_output_14_attendance <- data.frame(match.data(m.out_14_attendance))

# Undertake matching at age 15 (matching the age 16 entrants)
m.out_15_attendance = matchit(formula = alted_during ~ male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
                   + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                   + num_addresses_prior + mme_prior
                   + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                   + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                   + id + adhd + asd + ors_prior
                   + mother_mh + father_mh + child_mha
                   + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                   + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                   + teen_parent + mother_benefit_age15 + father_benefit_age15
                   + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                   + attendance_15_u + attendance_15_j + missing_attendance
                   ,data=AltEdPSM_15_attendance, exact=(~ birth_year), method="nearest")
summary(m.out_15_attendance, standardize=TRUE)

psm_output_15_attendance <- data.frame(match.data(m.out_15_attendance))

# Saving match data for models that incorporate attendance information
db_con = create_database_connection(database = "IDI_Sandpit")

new_table = copy_r_to_sql(
  db_connection = db_con,
  db = "[IDI_Sandpit]",
  schema = "[DL-MAA2021-60]",
  sql_table_name = "[ae_202210_psm_age12_output_attendance]",
  r_table_name = psm_output_12_attendance,
  OVERWRITE = TRUE
)

new_table = copy_r_to_sql(
  db_connection = db_con,
  db = "[IDI_Sandpit]",
  schema = "[DL-MAA2021-60]",
  sql_table_name = "[ae_202210_psm_age13_output_attendance]",
  r_table_name = psm_output_13_attendance,
  OVERWRITE = TRUE
)

new_table = copy_r_to_sql(
  db_connection = db_con,
  db = "[IDI_Sandpit]",
  schema = "[DL-MAA2021-60]",
  sql_table_name = "[ae_202210_psm_age14_output_attendance]",
  r_table_name = psm_output_14_attendance,
  OVERWRITE = TRUE
)

new_table = copy_r_to_sql(
  db_connection = db_con,
  db = "[IDI_Sandpit]",
  schema = "[DL-MAA2021-60]",
  sql_table_name = "[ae_202210_psm_age15_output_attendance]",
  r_table_name = psm_output_15_attendance,
  OVERWRITE = TRUE
)

close_database_connection(db_con)

# Remove PSM models from data environment
rm(m.out_12_attendance)
rm(m.out_13_attendance)
rm(m.out_14_attendance)
rm(m.out_15_attendance)

ggplot(psm_output_12_attendance, aes(x = distance, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) + 
  labs(fill='AE referral', x='Predicted probability (age 12)', y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/propensity_attend_12.png', width = 6, height = 4, units='in', dpi = 300)

ggplot(psm_output_13_attendance, aes(x = distance, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) + 
  labs(fill='AE referral', x='Predicted probability (age 13)', y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/propensity_attend_13.png', width = 6, height = 4, units='in', dpi = 300)

ggplot(psm_output_14_attendance, aes(x = distance, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) + 
  labs(fill='AE referral', x='Predicted probability (age 14)', y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/propensity_attend_14.png', width = 6, height = 4, units='in', dpi = 300)

ggplot(psm_output_15_attendance, aes(x = distance, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) + 
  labs(fill='AE referral', x='Predicted probability (age 15)', y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/propensity_attend_15.png', width = 6, height = 4, units='in', dpi = 300)


# Check the distribution of attendance again
ggplot(subset(psm_output_12_attendance,missing_attendance==0), aes(x = attendance_12_p, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) +
  labs(fill='AE referral', x="Attendance rate at age 12", y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/attendp_attend_12.png', width = 6, height = 4, units='in', dpi = 300)

ggplot(subset(psm_output_12_attendance,missing_attendance==0), aes(x = attendance_12_u, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) +
  labs(fill='AE referral', x="Unjustified absence rate at age 12", y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/attendu_attend_12.png', width = 6, height = 4, units='in', dpi = 300)

ggplot(subset(psm_output_13_attendance,missing_attendance==0), aes(x = attendance_13_p, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) +
  labs(fill='AE referral', x="Attendance rate at age 13", y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/attendp_attend_13.png', width = 6, height = 4, units='in', dpi = 300)

ggplot(subset(psm_output_13_attendance,missing_attendance==0), aes(x = attendance_13_u, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) +
  labs(fill='AE referral', x="Unjustified absence rate at age 13", y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/attendu_attend_13.png', width = 6, height = 4, units='in', dpi = 300)

ggplot(subset(psm_output_14_attendance,missing_attendance==0), aes(x = attendance_14_p, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) +
  labs(fill='AE referral', x="Attendance rate at age 14", y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/attendp_attend_14.png', width = 6, height = 4, units='in', dpi = 300)

ggplot(subset(psm_output_14_attendance,missing_attendance==0), aes(x = attendance_14_u, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) +
  labs(fill='AE referral', x="Unjustified absence rate at age 14", y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/attendu_attend_14.png', width = 6, height = 4, units='in', dpi = 300)

ggplot(subset(psm_output_15_attendance,missing_attendance==0), aes(x = attendance_15_p, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) +
  labs(fill='AE referral', x="Attendance rate at age 15", y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/attendp_attend_15.png', width = 6, height = 4, units='in', dpi = 300)

ggplot(subset(psm_output_15_attendance,missing_attendance==0), aes(x = attendance_15_u, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) +
  labs(fill='AE referral', x="Unjustified absence rate at age 15", y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/attendu_attend_15.png', width = 6, height = 4, units='in', dpi = 300)



#########################################################################
################ Sensitivity analysis - exact matching
#########################################################################

# Our preferred model only exact matches on birth cohort (for convenience of followup analysis more than anything else).
# However, there is a small but systematic imbalance here, where the control group has systematically fewer of these events
#   -Standdowns
#   -Suspensions
#   -NENs (attendance service referrals)
#   -Youth justice family group conferences
# These variables are conceptually tightly linked to AE referrals, and so this imbalance might be problematic for credibility.
# One way to solve this is by undertaking an exact matching processes - where control students are allocated based on having exactly
# the same distribution of data across all of these variables. However, this has several consequences:
#   1. The distribution of propensities between treatment and control groups have slight differences when exact matching is used
#   2. There are a small number of AE students that could not be assigned matched controls (because there are no other students 
#      with their exact profile of standdowns/suspensions/NENs), and are therefore dropped from the followup analysis.
# This trade-off probably cannot be adequately determined empirically, and so might be a call for ERO/the advisory group. 
# This means we should try our analysis both ways and make the call later (alternatively, if the results are not sensitive to
# whether or not exact matching is employed, then it doesn't really matter).

# Undertake matching at age 12 (matching the age 13 entrants)
m.out_12_exact = matchit(formula = alted_during ~ male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
                   + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                   + num_addresses_prior + mme_prior
                   + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                   + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                   + id + adhd + asd + ors_prior
                   + mother_mh + father_mh + child_mha 
                   + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                   + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                   + teen_parent + mother_benefit_age12 + father_benefit_age12
                   + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                   ,data=AltEdPSM_12, exact=(~ birth_year + standdown_n_prior + suspension_n_prior + attserv_n_prior + yj_fgc_prior), method="nearest")
# summary(m.out_12_exact, standardize=TRUE)

psm_output_12_exact <- data.frame(match.data(m.out_12_exact))

# Undertake matching at age 13 (matching the age 14 entrants)
m.out_13_exact = matchit(formula = alted_during ~ male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
                   + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                   + num_addresses_prior + mme_prior
                   + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                   + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                   + id + adhd + asd + ors_prior
                   + mother_mh + father_mh + child_mha 
                   + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                   + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                   + teen_parent + mother_benefit_age13 + father_benefit_age13
                   + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                   ,data=AltEdPSM_13, exact=(~ birth_year + standdown_n_prior + suspension_n_prior + attserv_n_prior + yj_fgc_prior), method="nearest")
# summary(m.out_13_exact, standardize=TRUE)

psm_output_13_exact <- data.frame(match.data(m.out_13_exact))

# Undertake matching at age 14 (matching the age 15 entrants)
m.out_14_exact = matchit(formula = alted_during ~ male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
                   + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                   + num_addresses_prior + mme_prior
                   + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                   + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                   + id + adhd + asd + ors_prior
                   + mother_mh + father_mh + child_mha 
                   + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                   + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                   + teen_parent + mother_benefit_age14 + father_benefit_age14
                   + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                   ,data=AltEdPSM_14, exact=(~ birth_year + standdown_n_prior + suspension_n_prior + attserv_n_prior + yj_fgc_prior), method="nearest")
# summary(m.out_14_exact, standardize=TRUE)

psm_output_14_exact <- data.frame(match.data(m.out_14_exact))

# Undertake matching at age 15 (matching the age 16 entrants)
m.out_15_exact = matchit(formula = alted_during ~ male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
                   + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                   + num_addresses_prior + mme_prior
                   + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                   + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                   + id + adhd + asd + ors_prior
                   + mother_mh + father_mh + child_mha
                   + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                   + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                   + teen_parent + mother_benefit_age15 + father_benefit_age15
                   + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                   ,data=AltEdPSM_15, exact=(~ birth_year + standdown_n_prior + suspension_n_prior + attserv_n_prior + yj_fgc_prior), method="nearest")
# summary(m.out_15_exact, standardize=TRUE)

psm_output_15_exact <- data.frame(match.data(m.out_15_exact))


ggplot(psm_output_12_exact, aes(x = distance, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) + 
  labs(fill='AE referral', x='Predicted probability (age 12)', y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/propensity_exact_12.png', width = 6, height = 4, units='in', dpi = 300)

ggplot(psm_output_13_exact, aes(x = distance, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) + 
  labs(fill='AE referral', x='Predicted probability (age 13)', y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/propensity_exact_13.png', width = 6, height = 4, units='in', dpi = 300)

ggplot(psm_output_14_exact, aes(x = distance, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) + 
  labs(fill='AE referral', x='Predicted probability (age 14)', y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/propensity_exact_14.png', width = 6, height = 4, units='in', dpi = 300)

ggplot(psm_output_15_exact, aes(x = distance, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) + 
  labs(fill='AE referral', x='Predicted probability (age 15)', y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/propensity_exact_15.png', width = 6, height = 4, units='in', dpi = 300)


# Check the distribution of attendance again
ggplot(subset(psm_output_12_exact,missing_attendance==0), aes(x = attendance_12_p, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) +
  labs(fill='AE referral', x="Attendance rate at age 12", y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/attendp_exact_12.png', width = 6, height = 4, units='in', dpi = 300)

ggplot(subset(psm_output_12_exact,missing_attendance==0), aes(x = attendance_12_u, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) +
  labs(fill='AE referral', x="Unjustified absence rate at age 12", y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/attendu_exact_12.png', width = 6, height = 4, units='in', dpi = 300)

ggplot(subset(psm_output_13_exact,missing_attendance==0), aes(x = attendance_13_p, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) +
  labs(fill='AE referral', x="Attendance rate at age 13", y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/attendp_exact_13.png', width = 6, height = 4, units='in', dpi = 300)

ggplot(subset(psm_output_13_exact,missing_attendance==0), aes(x = attendance_13_u, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) +
  labs(fill='AE referral', x="Unjustified absence rate at age 13", y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/attendu_exact_13.png', width = 6, height = 4, units='in', dpi = 300)

ggplot(subset(psm_output_14_exact,missing_attendance==0), aes(x = attendance_14_p, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) +
  labs(fill='AE referral', x="Attendance rate at age 14", y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/attendp_exact_14.png', width = 6, height = 4, units='in', dpi = 300)

ggplot(subset(psm_output_14_exact,missing_attendance==0), aes(x = attendance_14_u, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) +
  labs(fill='AE referral', x="Unjustified absence rate at age 14", y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/attendu_exact_14.png', width = 6, height = 4, units='in', dpi = 300)

ggplot(subset(psm_output_15_exact,missing_attendance==0), aes(x = attendance_15_p, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) +
  labs(fill='AE referral', x="Attendance rate at age 15", y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/attendp_exact_15.png', width = 6, height = 4, units='in', dpi = 300)

ggplot(subset(psm_output_15_exact,missing_attendance==0), aes(x = attendance_15_u, group=factor(alted_during), fill=factor(alted_during))) + 
  geom_density(alpha=0.4) +
  labs(fill='AE referral', x="Unjustified absence rate at age 15", y='Density') + 
  theme(axis.text.y=element_blank()) +
  ggsave('graphs/attendu_exact_15.png', width = 6, height = 4, units='in', dpi = 300)





# Saving match data for models that use exact matching on standdowns, suspensions, NENs, family group conferences
db_con = create_database_connection(database = "IDI_Sandpit")

new_table = copy_r_to_sql(
  db_connection = db_con,
  db = "[IDI_Sandpit]",
  schema = "[DL-MAA2021-60]",
  sql_table_name = "[ae_202210_psm_age12_output_exact]",
  r_table_name = psm_output_12_exact,
  OVERWRITE = TRUE
)

new_table = copy_r_to_sql(
  db_connection = db_con,
  db = "[IDI_Sandpit]",
  schema = "[DL-MAA2021-60]",
  sql_table_name = "[ae_202210_psm_age13_output_exact]",
  r_table_name = psm_output_13_exact,
  OVERWRITE = TRUE
)

new_table = copy_r_to_sql(
  db_connection = db_con,
  db = "[IDI_Sandpit]",
  schema = "[DL-MAA2021-60]",
  sql_table_name = "[ae_202210_psm_age14_output_exact]",
  r_table_name = psm_output_14_exact,
  OVERWRITE = TRUE
)

new_table = copy_r_to_sql(
  db_connection = db_con,
  db = "[IDI_Sandpit]",
  schema = "[DL-MAA2021-60]",
  sql_table_name = "[ae_202210_psm_age15_output_exact]",
  r_table_name = psm_output_15_exact,
  OVERWRITE = TRUE
)

close_database_connection(db_con)

# Remove PSM models from data environment
rm(m.out_12_exact)
rm(m.out_13_exact)
rm(m.out_14_exact)
rm(m.out_15_exact)



#########################################################################
################ Saving all matched files to sandpit
#########################################################################

source("utility_functions.R")
source("dbplyr_helper_functions.R")
db_con = create_database_connection(database = "IDI_Sandpit")

# Saving preferred matching data
new_table = copy_r_to_sql(
  db_connection = db_con,
  db = "[IDI_Sandpit]",
  schema = "[DL-MAA2021-60]",
  sql_table_name = "[ae_202210_psm_age12_output]",
  r_table_name = psm_output_12,
  OVERWRITE = TRUE
)

new_table = copy_r_to_sql(
  db_connection = db_con,
  db = "[IDI_Sandpit]",
  schema = "[DL-MAA2021-60]",
  sql_table_name = "[ae_202210_psm_age13_output]",
  r_table_name = psm_output_13,
  OVERWRITE = TRUE
)

new_table = copy_r_to_sql(
  db_connection = db_con,
  db = "[IDI_Sandpit]",
  schema = "[DL-MAA2021-60]",
  sql_table_name = "[ae_202210_psm_age14_output]",
  r_table_name = psm_output_14,
  OVERWRITE = TRUE
)

new_table = copy_r_to_sql(
  db_connection = db_con,
  db = "[IDI_Sandpit]",
  schema = "[DL-MAA2021-60]",
  sql_table_name = "[ae_202210_psm_age15_output]",
  r_table_name = psm_output_15,
  OVERWRITE = TRUE
)

# Saving match data for models that use exact matching on standdowns, suspensions, NENs, family group conferences
new_table = copy_r_to_sql(
  db_connection = db_con,
  db = "[IDI_Sandpit]",
  schema = "[DL-MAA2021-60]",
  sql_table_name = "[ae_202210_psm_age12_output_exact]",
  r_table_name = psm_output_12_exact,
  OVERWRITE = TRUE
)

new_table = copy_r_to_sql(
  db_connection = db_con,
  db = "[IDI_Sandpit]",
  schema = "[DL-MAA2021-60]",
  sql_table_name = "[ae_202210_psm_age13_output_exact]",
  r_table_name = psm_output_13_exact,
  OVERWRITE = TRUE
)

new_table = copy_r_to_sql(
  db_connection = db_con,
  db = "[IDI_Sandpit]",
  schema = "[DL-MAA2021-60]",
  sql_table_name = "[ae_202210_psm_age14_output_exact]",
  r_table_name = psm_output_14_exact,
  OVERWRITE = TRUE
)

new_table = copy_r_to_sql(
  db_connection = db_con,
  db = "[IDI_Sandpit]",
  schema = "[DL-MAA2021-60]",
  sql_table_name = "[ae_202210_psm_age15_output_exact]",
  r_table_name = psm_output_15_exact,
  OVERWRITE = TRUE
)


# Saving match data for models that incorporate attendance information
new_table = copy_r_to_sql(
  db_connection = db_con,
  db = "[IDI_Sandpit]",
  schema = "[DL-MAA2021-60]",
  sql_table_name = "[ae_202210_psm_age12_output_attendance]",
  r_table_name = psm_output_12_attendance,
  OVERWRITE = TRUE
)

new_table = copy_r_to_sql(
  db_connection = db_con,
  db = "[IDI_Sandpit]",
  schema = "[DL-MAA2021-60]",
  sql_table_name = "[ae_202210_psm_age13_output_attendance]",
  r_table_name = psm_output_13_attendance,
  OVERWRITE = TRUE
)

new_table = copy_r_to_sql(
  db_connection = db_con,
  db = "[IDI_Sandpit]",
  schema = "[DL-MAA2021-60]",
  sql_table_name = "[ae_202210_psm_age14_output_attendance]",
  r_table_name = psm_output_14_attendance,
  OVERWRITE = TRUE
)

new_table = copy_r_to_sql(
  db_connection = db_con,
  db = "[IDI_Sandpit]",
  schema = "[DL-MAA2021-60]",
  sql_table_name = "[ae_202210_psm_age15_output_attendance]",
  r_table_name = psm_output_15_attendance,
  OVERWRITE = TRUE
)

close_database_connection(db_con)




#########################################################################
################ Robustness
#########################################################################

# Robustness: Marc DeBoer method
# The logic here is that if the PSM process is effective, it has simulated an RCT. This means that if we run a 
# predictive model (using the same matching characteristics as predictors) on only the treated and matched learners,
# the model should struggle to accurately predict (ie have very low area under the ROC).

robustdf_12 <- psm_output_12

logit_robust_12 <- glm(alted_during ~ male + eth_maori + sole_maori + eth_pacific + eth_asian + eth_melaa + eth_other + born_in_nz
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
                    ,data=robustdf_12, family="binomial")

prob_robust_12 = predict(logit_robust_12, type=c("response"))
robustdf_12$prob_robust = prob_robust_12
roc_logit_12_robust <- roc(alted_during ~ prob_robust_12, data=robustdf_12)
png('graphs/auc_robust_12.png', width=800, height=800, res=300, pointsize=5)
plot(roc_logit_12_robust, print.auc=TRUE)
dev.off()

tempauc <- auc(roc_logit_12_robust)
output_robust_12 <- data.frame(
  model = "Age 12",
  iteration = 0,
  auc = round(tempauc,digits = 4)
)

# Producing distribution of comparable ROCs (when generating treatment randomly) for age 12 model
set.seed(21212)
iterlist <- c(1:100)

# NOTE: In order to generate a randomly allocated treatment variable, the below code requires the number of rows in the dataframe.
# This is equivalent to a count, so I have suppressed this as "[COUNT]". "[COUNT/2]" represents have the number of rows in the dataframe.
for (i in iterlist) {
  assign(paste("random_",i,sep=""), sample(x=c(rep(1,[COUNT/2]),rep(0,[COUNT/2])),size=[COUNT],replace=FALSE))
  eval(parse(text=paste("robustdf_12$random_",i," = random_",i,sep="")))
  
  logit_random <- glm(eval(parse(text=paste("random_",i,sep=""))) ~ male + eth_maori + sole_maori + eth_pacific + eth_asian + eth_melaa + eth_other + born_in_nz
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
                      ,data=robustdf_12, family="binomial")
  prob_random = predict(logit_random, type=c("response"))
  robustdf_12$prob_random = prob_random
  roc_random <- roc(eval(parse(text=paste("random_",i,sep=""))) ~ prob_random, data=robustdf_12)
  tempauc <- auc(roc_random)
  output_random <- data.frame(
    model = "Age 12",
    iteration = i,
    auc = round(tempauc,digits = 4)
  )
  output_robust_12 <- rbind(output_robust_12,output_random)
}

# Now age 13
robustdf_13 <- psm_output_13

logit_robust_13 <- glm(alted_during ~ male + eth_maori + sole_maori + eth_pacific + eth_asian + eth_melaa + eth_other + born_in_nz
                       + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                       + num_addresses_prior + mme_prior
                       + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                       + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                       + id + adhd + asd + ors_prior
                       + factor(birth_year)
                       + mother_mh + father_mh + child_mha
                       + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                       + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                       + teen_parent + mother_benefit_age13 + father_benefit_age13
                       + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                       ,data=robustdf_13, family="binomial")

prob_robust_13 = predict(logit_robust_13, type=c("response"))
robustdf_13$prob_robust = prob_robust_13
roc_logit_13_robust <- roc(alted_during ~ prob_robust_13, data=robustdf_13)
png('graphs/auc_robust_13.png', width=800, height=800, res=300, pointsize=5)
plot(roc_logit_13_robust, print.auc=TRUE)
dev.off()

tempauc <- auc(roc_logit_13_robust)
output_robust_13 <- data.frame(
  model = "Age 13",
  iteration = 0,
  auc = round(tempauc,digits = 4)
)

# NOTE: In order to generate a randomly allocated treatment variable, the below code requires the number of rows in the dataframe.
# This is equivalent to a count, so I have suppressed this as "[COUNT]". "[COUNT/2]" represents have the number of rows in the dataframe.
for (i in iterlist) {
  assign(paste("random_",i,sep=""), sample(x=c(rep(1,[COUNT/2]),rep(0,[COUNT/2])),size=[COUNT],replace=FALSE))
  eval(parse(text=paste("robustdf_13$random_",i," = random_",i,sep="")))
  
  logit_random <- glm(eval(parse(text=paste("random_",i,sep=""))) ~ male + eth_maori + sole_maori + eth_pacific + eth_asian + eth_melaa + eth_other + born_in_nz
                      + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                      + num_addresses_prior + mme_prior
                      + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                      + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                      + id + adhd + asd + ors_prior
                      + factor(birth_year)
                      + mother_mh + father_mh + child_mha
                      + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                      + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                      + teen_parent + mother_benefit_age13 + father_benefit_age13
                      + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                      ,data=robustdf_13, family="binomial")
  prob_random = predict(logit_random, type=c("response"))
  robustdf_13$prob_random = prob_random
  roc_random <- roc(eval(parse(text=paste("random_",i,sep=""))) ~ prob_random, data=robustdf_13)
  tempauc <- auc(roc_random)
  output_random <- data.frame(
    model = "Age 13",
    iteration = i,
    auc = round(tempauc,digits = 4)
  )
  output_robust_13 <- rbind(output_robust_13,output_random)
}

# Now age 14
robustdf_14 <- psm_output_14

logit_robust_14 <- glm(alted_during ~ male + eth_maori + sole_maori + eth_pacific + eth_asian + eth_melaa + eth_other + born_in_nz
                       + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                       + num_addresses_prior + mme_prior
                       + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                       + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                       + id + adhd + asd + ors_prior
                       + factor(birth_year)
                       + mother_mh + father_mh + child_mha
                       + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                       + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                       + teen_parent + mother_benefit_age14 + father_benefit_age14
                       + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                       ,data=robustdf_14, family="binomial")

prob_robust_14 = predict(logit_robust_14, type=c("response"))
robustdf_14$prob_robust = prob_robust_14
roc_logit_14_robust <- roc(alted_during ~ prob_robust_14, data=robustdf_14)
png('graphs/auc_robust_14.png', width=800, height=800, res=300, pointsize=5)
plot(roc_logit_14_robust, print.auc=TRUE)
dev.off()

tempauc <- auc(roc_logit_14_robust)
output_robust_14 <- data.frame(
  model = "Age 14",
  iteration = 0,
  auc = round(tempauc,digits = 4)
)

# NOTE: In order to generate a randomly allocated treatment variable, the below code requires the number of rows in the dataframe.
# This is equivalent to a count, so I have suppressed this as "[COUNT]". "[COUNT/2]" represents have the number of rows in the dataframe.
for (i in iterlist) {
  assign(paste("random_",i,sep=""), sample(x=c(rep(1,[COUNT/2]),rep(0,[COUNT/2])),size=[COUNT],replace=FALSE))
  eval(parse(text=paste("robustdf_14$random_",i," = random_",i,sep="")))
  
  logit_random <- glm(eval(parse(text=paste("random_",i,sep=""))) ~ male + eth_maori + sole_maori + eth_pacific + eth_asian + eth_melaa + eth_other + born_in_nz
                      + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                      + num_addresses_prior + mme_prior
                      + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                      + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                      + id + adhd + asd + ors_prior
                      + factor(birth_year)
                      + mother_mh + father_mh + child_mha
                      + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                      + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                      + teen_parent + mother_benefit_age14 + father_benefit_age14
                      + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                      ,data=robustdf_14, family="binomial")
  prob_random = predict(logit_random, type=c("response"))
  robustdf_14$prob_random = prob_random
  roc_random <- roc(eval(parse(text=paste("random_",i,sep=""))) ~ prob_random, data=robustdf_14)
  tempauc <- auc(roc_random)
  output_random <- data.frame(
    model = "Age 14",
    iteration = i,
    auc = round(tempauc,digits = 4)
  )
  output_robust_14 <- rbind(output_robust_14,output_random)
}

# Now age 15
robustdf_15 <- psm_output_15

logit_robust_15 <- glm(alted_during ~ male + eth_maori + sole_maori + eth_pacific + eth_asian + eth_melaa + eth_other + born_in_nz
                       + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                       + num_addresses_prior + mme_prior
                       + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                       + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                       + id + adhd + asd + ors_prior
                       + factor(birth_year)
                       + mother_mh + father_mh + child_mha
                       + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                       + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                       + teen_parent + mother_benefit_age15 + father_benefit_age15
                       + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                       ,data=robustdf_15, family="binomial")

prob_robust_15 = predict(logit_robust_15, type=c("response"))
robustdf_15$prob_robust = prob_robust_15
roc_logit_15_robust <- roc(alted_during ~ prob_robust_15, data=robustdf_15)
png('graphs/auc_robust_15.png', width=800, height=800, res=300, pointsize=5)
plot(roc_logit_15_robust, print.auc=TRUE)
dev.off()

tempauc <- auc(roc_logit_15_robust)
output_robust_15 <- data.frame(
  model = "Age 15",
  iteration = 0,
  auc = round(tempauc,digits = 4)
)

# NOTE: In order to generate a randomly allocated treatment variable, the below code requires the number of rows in the dataframe.
# This is equivalent to a count, so I have suppressed this as "[COUNT]". "[COUNT/2]" represents have the number of rows in the dataframe.
for (i in iterlist) {
  assign(paste("random_",i,sep=""), sample(x=c(rep(1,[COUNT/2]),rep(0,[COUNT/2])),size=[COUNT],replace=FALSE))
  eval(parse(text=paste("robustdf_15$random_",i," = random_",i,sep="")))
  
  logit_random <- glm(eval(parse(text=paste("random_",i,sep=""))) ~ male + eth_maori + sole_maori + eth_pacific + eth_asian + eth_melaa + eth_other + born_in_nz
                      + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                      + num_addresses_prior + mme_prior
                      + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                      + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                      + id + adhd + asd + ors_prior
                      + factor(birth_year)
                      + mother_mh + father_mh + child_mha
                      + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                      + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                      + teen_parent + mother_benefit_age15 + father_benefit_age15
                      + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                      ,data=robustdf_15, family="binomial")
  prob_random = predict(logit_random, type=c("response"))
  robustdf_15$prob_random = prob_random
  roc_random <- roc(eval(parse(text=paste("random_",i,sep=""))) ~ prob_random, data=robustdf_15)
  tempauc <- auc(roc_random)
  output_random <- data.frame(
    model = "Age 15",
    iteration = i,
    auc = round(tempauc,digits = 4)
  )
  output_robust_15 <- rbind(output_robust_15,output_random)
}

# Remove objects from data environment
rm(logit_robust_12)
rm(logit_robust_13)
rm(logit_robust_14)
rm(logit_robust_15)
rm(roc_logit_12_robust)
rm(roc_logit_13_robust)
rm(roc_logit_14_robust)
rm(roc_logit_15_robust)

# Save robustness checks
write.csv(output_robust_12,file="output_robustness_auc_12.csv", row.names=FALSE)
write.csv(output_robust_13,file="output_robustness_auc_13.csv", row.names=FALSE)
write.csv(output_robust_14,file="output_robustness_auc_14.csv", row.names=FALSE)
write.csv(output_robust_15,file="output_robustness_auc_15.csv", row.names=FALSE)


# Robustness: Anran Zhao method
# The logic is that we run a separate matching process without excluding the people who later get referred to AE.
# If all of our matched control group in this sample are composed of people who later receive AE, then this is evidence that AE people
# are systematically different from non-AE people. On the other hand, if our control group is mostly unchanged and still composed of
# "never AE" people, then our matching process is relatively robust.

AZrobust_12 <- sqlQuery(con,"select * from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_psm_age12_az_robust]")
AZrobust_13 <- sqlQuery(con,"select * from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_psm_age13_az_robust]")
AZrobust_14 <- sqlQuery(con,"select * from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_psm_age14_az_robust]")

AZrobust_12[is.na(AZrobust_12)] <- 0
AZrobust_13[is.na(AZrobust_13)] <- 0
AZrobust_14[is.na(AZrobust_14)] <- 0

AZrobust_12 <- AZrobust_12 %>% mutate(alted_during = case_when(alted_later == 1 ~ as.integer(0), TRUE ~ alted_during))
AZrobust_13 <- AZrobust_13 %>% mutate(alted_during = case_when(alted_later == 1 ~ as.integer(0), TRUE ~ alted_during))
AZrobust_14 <- AZrobust_14 %>% mutate(alted_during = case_when(alted_later == 1 ~ as.integer(0), TRUE ~ alted_during))

# Try regular 1:1 matching
m.out_12_azrobust = matchit(formula = alted_during ~ male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
                   + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                   + num_addresses_prior + mme_prior
                   + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                   + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                   + id + adhd + asd + ors_prior
                   + mother_mh + father_mh + child_mha
                   + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                   + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                   + teen_parent + mother_benefit_age12 + father_benefit_age12
                   + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                   ,data=AZrobust_12, exact=(~ birth_year), method="nearest")
summary(m.out_12_azrobust, standardize=TRUE)
psm_output_12_azrobust <- data.frame(match.data(m.out_12_azrobust))

m.out_13_azrobust = matchit(formula = alted_during ~ male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
                            + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                            + num_addresses_prior + mme_prior
                            + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                            + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                            + id + adhd + asd + ors_prior
                            + mother_mh + father_mh + child_mha
                            + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                            + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                            + teen_parent + mother_benefit_age13 + father_benefit_age13
                            + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                            ,data=AZrobust_13, exact=(~ birth_year), method="nearest")
summary(m.out_13_azrobust, standardize=TRUE)
psm_output_13_azrobust <- data.frame(match.data(m.out_13_azrobust))

m.out_14_azrobust = matchit(formula = alted_during ~ male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
                            + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                            + num_addresses_prior + mme_prior
                            + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                            + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                            + id + adhd + asd + ors_prior
                            + mother_mh + father_mh + child_mha
                            + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                            + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                            + teen_parent + mother_benefit_age14 + father_benefit_age14
                            + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                            ,data=AZrobust_14, exact=(~ birth_year), method="nearest")
summary(m.out_14_azrobust, standardize=TRUE)
psm_output_14_azrobust <- data.frame(match.data(m.out_14_azrobust))

table(psm_output_12_azrobust$alted_during,psm_output_12_azrobust$alted_later, useNA="always")
table(psm_output_13_azrobust$alted_during,psm_output_13_azrobust$alted_later, useNA="always")
table(psm_output_14_azrobust$alted_during,psm_output_14_azrobust$alted_later, useNA="always")

rm(m.out_12_azrobust)
rm(m.out_13_azrobust)
rm(m.out_14_azrobust)

# Try variable ratio matching
m.out_12_azrobust_many = matchit(formula = alted_during ~ male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
                            + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                            + num_addresses_prior + mme_prior
                            + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                            + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                            + id + adhd + asd + ors_prior
                            + mother_mh + father_mh + child_mha
                            + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                            + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                            + teen_parent + mother_benefit_age12 + father_benefit_age12
                            + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                            ,data=AZrobust_12, exact=(~ birth_year), method="nearest", min.controls=1, max.controls=5, ratio=3)
summary(m.out_12_azrobust_many, standardize=TRUE)
psm_output_12_azrobust_many <- data.frame(match.data(m.out_12_azrobust_many))

m.out_13_azrobust_many = matchit(formula = alted_during ~ male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
                            + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                            + num_addresses_prior + mme_prior
                            + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                            + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                            + id + adhd + asd + ors_prior
                            + mother_mh + father_mh + child_mha
                            + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                            + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                            + teen_parent + mother_benefit_age13 + father_benefit_age13
                            + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                            ,data=AZrobust_13, exact=(~ birth_year), method="nearest", min.controls=1, max.controls=5, ratio=3)
summary(m.out_13_azrobust_many, standardize=TRUE)
psm_output_13_azrobust_many <- data.frame(match.data(m.out_13_azrobust_many))

m.out_14_azrobust_many = matchit(formula = alted_during ~ male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
                            + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                            + num_addresses_prior + mme_prior
                            + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                            + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                            + id + adhd + asd + ors_prior
                            + mother_mh + father_mh + child_mha
                            + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                            + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                            + teen_parent + mother_benefit_age14 + father_benefit_age14
                            + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                            ,data=AZrobust_14, exact=(~ birth_year), method="nearest", min.controls=1, max.controls=5, ratio=3)
summary(m.out_14_azrobust_many, standardize=TRUE)
psm_output_14_azrobust_many <- data.frame(match.data(m.out_14_azrobust_many))

table(psm_output_12_azrobust_many$alted_during,psm_output_12_azrobust_many$alted_later, useNA="always")
table(psm_output_13_azrobust_many$alted_during,psm_output_13_azrobust_many$alted_later, useNA="always")
table(psm_output_14_azrobust_many$alted_during,psm_output_14_azrobust_many$alted_later, useNA="always")

rm(m.out_12_azrobust_many)
rm(m.out_13_azrobust_many)
rm(m.out_14_azrobust_many)


# Robustness: Stuart McNaughton method
# See the sql code for an explanation of the logic of this method.

SMrobust_12 <- sqlQuery(con,"select * from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_psm_age12_sm_robust]")
SMrobust_13 <- sqlQuery(con,"select * from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_psm_age13_sm_robust]")
SMrobust_14 <- sqlQuery(con,"select * from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_psm_age14_sm_robust]")
SMrobust_15 <- sqlQuery(con,"select * from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_psm_age15_sm_robust]")

SMrobust_12[is.na(SMrobust_12)] <- 0
SMrobust_13[is.na(SMrobust_13)] <- 0
SMrobust_14[is.na(SMrobust_14)] <- 0
SMrobust_15[is.na(SMrobust_15)] <- 0

# Undertake matching at age 12 (matching the age 13 entrants)
m.out_12_smrobust = matchit(formula = alted_during ~ male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
                   + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                   + num_addresses_prior + mme_prior
                   + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                   + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                   + id + adhd + asd + ors_prior
                   + mother_mh + father_mh + child_mha 
                   + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                   + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                   + teen_parent + mother_benefit_age12 + father_benefit_age12
                   + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                   ,data=SMrobust_12, exact=(~ birth_year), method="nearest")
summary(m.out_12_smrobust, standardize=TRUE)

psm_output_12_smrobust <- data.frame(match.data(m.out_12_smrobust))

# Undertake matching at age 13 (matching the age 14 entrants)
m.out_13_smrobust = matchit(formula = alted_during ~ male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
                   + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                   + num_addresses_prior + mme_prior
                   + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                   + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                   + id + adhd + asd + ors_prior
                   + mother_mh + father_mh + child_mha 
                   + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                   + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                   + teen_parent + mother_benefit_age13 + father_benefit_age13
                   + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                   ,data=SMrobust_13, exact=(~ birth_year), method="nearest")
summary(m.out_13_smrobust, standardize=TRUE)

psm_output_13_smrobust <- data.frame(match.data(m.out_13_smrobust))

# Undertake matching at age 14 (matching the age 15 entrants)
m.out_14_smrobust = matchit(formula = alted_during ~ male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
                   + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                   + num_addresses_prior + mme_prior
                   + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                   + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                   + id + adhd + asd + ors_prior
                   + mother_mh + father_mh + child_mha 
                   + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                   + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                   + teen_parent + mother_benefit_age14 + father_benefit_age14
                   + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                   ,data=SMrobust_14, exact=(~ birth_year), method="nearest")
summary(m.out_14_smrobust, standardize=TRUE)

psm_output_14_smrobust <- data.frame(match.data(m.out_14_smrobust))

# Undertake matching at age 15 (matching the age 16 entrants)
m.out_15_smrobust = matchit(formula = alted_during ~ male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
                   + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                   + num_addresses_prior + mme_prior
                   + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                   + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                   + id + adhd + asd + ors_prior
                   + mother_mh + father_mh + child_mha
                   + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                   + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                   + teen_parent + mother_benefit_age15 + father_benefit_age15
                   + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                   ,data=SMrobust_15, exact=(~ birth_year), method="nearest")
summary(m.out_15_smrobust, standardize=TRUE)

psm_output_15_smrobust <- data.frame(match.data(m.out_15_smrobust))


SMrobust_12_attendance <- sqlQuery(con,"select * from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_psm_age12_sm_attendance]")
SMrobust_13_attendance <- sqlQuery(con,"select * from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_psm_age13_sm_attendance]")
SMrobust_14_attendance <- sqlQuery(con,"select * from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_psm_age14_sm_attendance]")
SMrobust_15_attendance <- sqlQuery(con,"select * from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_psm_age15_sm_attendance]")

SMrobust_12_attendance[is.na(SMrobust_12_attendance)] <- 0
SMrobust_13_attendance[is.na(SMrobust_13_attendance)] <- 0
SMrobust_14_attendance[is.na(SMrobust_14_attendance)] <- 0
SMrobust_15_attendance[is.na(SMrobust_15_attendance)] <- 0

# Undertake matching at age 12 (matching the age 13 entrants)
m.out_12_smrobust_attendance = matchit(formula = alted_during ~ male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
                            + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                            + num_addresses_prior + mme_prior
                            + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                            + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                            + id + adhd + asd + ors_prior
                            + mother_mh + father_mh + child_mha 
                            + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                            + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                            + teen_parent + mother_benefit_age12 + father_benefit_age12
                            + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                            ,data=SMrobust_12_attendance, exact=(~ birth_year), method="nearest")
summary(m.out_12_smrobust_attendance, standardize=TRUE)

psm_output_12_smrobust_attendance <- data.frame(match.data(m.out_12_smrobust_attendance))

# Undertake matching at age 13 (matching the age 14 entrants)
m.out_13_smrobust_attendance = matchit(formula = alted_during ~ male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
                            + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                            + num_addresses_prior + mme_prior
                            + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                            + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                            + id + adhd + asd + ors_prior
                            + mother_mh + father_mh + child_mha 
                            + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                            + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                            + teen_parent + mother_benefit_age13 + father_benefit_age13
                            + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                            ,data=SMrobust_13_attendance, exact=(~ birth_year), method="nearest")
summary(m.out_13_smrobust_attendance, standardize=TRUE)

psm_output_13_smrobust_attendance <- data.frame(match.data(m.out_13_smrobust_attendance))

# Undertake matching at age 14 (matching the age 15 entrants)
m.out_14_smrobust_attendance = matchit(formula = alted_during ~ male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
                            + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                            + num_addresses_prior + mme_prior
                            + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                            + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                            + id + adhd + asd + ors_prior
                            + mother_mh + father_mh + child_mha 
                            + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                            + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                            + teen_parent + mother_benefit_age14 + father_benefit_age14
                            + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                            ,data=SMrobust_14, exact=(~ birth_year), method="nearest")
summary(m.out_14_smrobust_attendance, standardize=TRUE)

psm_output_14_smrobust_attendance <- data.frame(match.data(m.out_14_smrobust_attendance))

# Undertake matching at age 15 (matching the age 16 entrants)
m.out_15_smrobust_attendance = matchit(formula = alted_during ~ male + eth_maori + eth_pacific + eth_asian + eth_melaa + eth_other + sole_maori + born_in_nz
                            + sole_birth_parent + missing_address + missing_schools + missing_mother + missing_father + missing_motherinc + missing_fatherinc
                            + num_addresses_prior + mme_prior
                            + factor(standdown_n_prior) + factor(suspension_n_prior) + factor(attserv_n_prior)
                            + ot_report_prior + ot_invest_prior + yj_fgc_prior + ot_place_prior
                            + id + adhd + asd + ors_prior
                            + mother_mh + father_mh + child_mha
                            + factor(first_school_region) + factor(mother_qual_cat) + factor(father_qual_cat)
                            + factor(n_nonstructuralchanges_prior) + first_school_decile + first_nzdep
                            + teen_parent + mother_benefit_age15 + father_benefit_age15
                            + mother_custodial_prior+ mother_community_prior + father_custodial_prior + father_community_prior
                            ,data=SMrobust_15_attendance, exact=(~ birth_year), method="nearest")
summary(m.out_15_smrobust_attendance, standardize=TRUE)

psm_output_15_smrobust_attendance <- data.frame(match.data(m.out_15_smrobust_attendance))

# Save robustness tables back to sandpit

source("utility_functions.R")
source("dbplyr_helper_functions.R")
db_con = create_database_connection(database = "IDI_Sandpit")

# Saving preferred matching data
new_table = copy_r_to_sql(
  db_connection = db_con,
  db = "[IDI_Sandpit]",
  schema = "[DL-MAA2021-60]",
  sql_table_name = "[ae_202210_psm_age12_output_smrobust]",
  r_table_name = psm_output_12_smrobust,
  OVERWRITE = TRUE
)

new_table = copy_r_to_sql(
  db_connection = db_con,
  db = "[IDI_Sandpit]",
  schema = "[DL-MAA2021-60]",
  sql_table_name = "[ae_202210_psm_age13_output_smrobust]",
  r_table_name = psm_output_13_smrobust,
  OVERWRITE = TRUE
)

new_table = copy_r_to_sql(
  db_connection = db_con,
  db = "[IDI_Sandpit]",
  schema = "[DL-MAA2021-60]",
  sql_table_name = "[ae_202210_psm_age14_output_smrobust]",
  r_table_name = psm_output_14_smrobust,
  OVERWRITE = TRUE
)

new_table = copy_r_to_sql(
  db_connection = db_con,
  db = "[IDI_Sandpit]",
  schema = "[DL-MAA2021-60]",
  sql_table_name = "[ae_202210_psm_age15_output_smrobust]",
  r_table_name = psm_output_15_smrobust,
  OVERWRITE = TRUE
)

# Saving match data for models that incorporate attendance information
new_table = copy_r_to_sql(
  db_connection = db_con,
  db = "[IDI_Sandpit]",
  schema = "[DL-MAA2021-60]",
  sql_table_name = "[ae_202210_psm_age12_output_smattendance]",
  r_table_name = psm_output_12_smrobust_attendance,
  OVERWRITE = TRUE
)

new_table = copy_r_to_sql(
  db_connection = db_con,
  db = "[IDI_Sandpit]",
  schema = "[DL-MAA2021-60]",
  sql_table_name = "[ae_202210_psm_age13_output_smattendance]",
  r_table_name = psm_output_13_smrobust_attendance,
  OVERWRITE = TRUE
)

new_table = copy_r_to_sql(
  db_connection = db_con,
  db = "[IDI_Sandpit]",
  schema = "[DL-MAA2021-60]",
  sql_table_name = "[ae_202210_psm_age14_output_smattendance]",
  r_table_name = psm_output_14_smrobust_attendance,
  OVERWRITE = TRUE
)

new_table = copy_r_to_sql(
  db_connection = db_con,
  db = "[IDI_Sandpit]",
  schema = "[DL-MAA2021-60]",
  sql_table_name = "[ae_202210_psm_age15_output_smattendance]",
  r_table_name = psm_output_15_smrobust_attendance,
  OVERWRITE = TRUE
)

close_database_connection(db_con)



# Save workspace image
save.image("/nas/DataLab/MAA/MAA2021-60/Alternative_Education/AltEd after PSM 2023-03-27.RData")
# 
# load("/nas/DataLab/MAA/MAA2021-60/Alternative_Education/AltEd after PSM 2023-03-27.RData")
