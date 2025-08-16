# For those cohorts including patients without myocardial injury
# Those cohorts including only patients with myocardial injury do not need this script. 

# Subdistribution hazards -------------------------------------------------

# Subset to complete cases for adjusted model variables
if (model == "primary") {
  df_sa_sh <- df_sa[complete.cases(df_sa[, c("udmi4", "age", "sex", "gfr")]), ]
} else if (model == "secondary") {
  df_sa_sh <- df_sa[complete.cases(df_sa[, c("udmi4", "age", "sex", "gfr", "PrevIHD", "DM")]), ]
}

# Time and status 
if (fu_time == "1y") {
  ftime_sa_sh <- df_sa_sh[[time]]
  fstatus_sa_sh <- df_sa_sh[[status]]  # Numeric: 0 = censored, 1 = MACE, 2 = Non-CV death
} else if (fu_time == "5y") {
  ftime_sa_sh <- df_sa_sh[[time]]
  fstatus_sa_sh <- df_sa_sh[[status]]  # Numeric: 0 = censored, 1 = MACE, 2 = Non-CV death
} 

# Adjusted model matrices for crr function
if (model == "primary") {
  adj_mat_sa_sh <- model.matrix(~ udmi4 + age + sex + gfr, data = df_sa_sh)[, -1]
} else if (model == "secondary") {
  adj_mat_sa_sh <- model.matrix(~ udmi4 + age + sex + gfr + PrevIHD + DM , data = df_sa_sh)[, -1]
}

## Adjusted Fine-Gray model----

### MACE model (status == 1)----
fg_mace_adj <- crr(ftime = ftime_sa_sh, fstatus = fstatus_sa_sh, cov1 = adj_mat_sa_sh, failcode = 1, cencode = 0)

### Non-CV death model (status == 2)----
fg_ncv_adj <- crr(ftime = ftime_sa_sh, fstatus = fstatus_sa_sh, cov1 = adj_mat_sa_sh, failcode = 2, cencode = 0)

## Results----
tidy_crr(fg_mace_adj)
tidy_crr(fg_ncv_adj)

## Save----
write.csv(tidy_crr(fg_mace_adj), file = glue("Tables/sh/{fu_time}/fg_mace_sa_adj_{model}_model_fu_{fu_time}_{cohort}.csv"), row.names = FALSE)
write.csv(tidy_crr(fg_ncv_adj), file = glue("Tables/sh/{fu_time}/fg_ncv_sa_adj_{model}_model_fu_{fu_time}_{cohort}.csv"), row.names = FALSE)
print("Subdistribution hazards model saved as .csv file")