# Code for running subdistribution hazards model
# Output produces table with coefficients for: 
# -- status 1 (AMI or CV death)
# -- status 2 (non CV death)

# Subdistribution hazards -------------------------------------------------

# Subset to complete cases for adjusted model variables
if (model == "primary") {
  df_sh <- df[complete.cases(df[, c("udmi4", "age", "sex", "gfr")]), ]
} else if (model == "secondary") {
  df_sh <- df[complete.cases(df[, c("udmi4", "age", "sex", "gfr", "PrevIHD", "DM")]), ]
}

# Time and status 
if (fu_time == "1y") {
  ftime_sh <- df_sh[[time]]
  fstatus_sh <- df_sh[[status]]  # Numeric: 0 = censored, 1 = MACE, 2 = Non-CV death
} else if (fu_time == "5y") {
  ftime_sh <- df_sh[[time]]
  fstatus_sh <- df_sh[[status]]  # Numeric: 0 = censored, 1 = MACE, 2 = Non-CV death
} 

# Adjusted model matrices for crr function
if (model == "primary") {
  adj_mat_sh <- model.matrix(~ udmi4 + age + sex + gfr, data = df_sh)[, -1]
} else if (model == "secondary") {
  adj_mat_sh <- model.matrix(~ udmi4 + age + sex + gfr + PrevIHD + DM , data = df_sh)[, -1]
}

## Adjusted Fine-Gray model----

### MACE model (status == 1)----
fg_mace_adj <- crr(ftime = ftime_sh, fstatus = fstatus_sh, cov1 = adj_mat_sh, failcode = 1, cencode = 0)

### Non-CV death model (status == 2)----
fg_ncv_adj <- crr(ftime = ftime_sh, fstatus = fstatus_sh, cov1 = adj_mat_sh, failcode = 2, cencode = 0)

## Results----
tidy_crr(fg_mace_adj)
tidy_crr(fg_ncv_adj)

## Save----
write.csv(tidy_crr(fg_mace_adj), file = glue("Tables/sh/{fu_time}/fg_mace_adj_{model}_model_fu_{fu_time}_{cohort}.csv"), row.names = FALSE)
write.csv(tidy_crr(fg_ncv_adj), file = glue("Tables/sh/{fu_time}/fg_ncv_adj_{model}_model_fu_{fu_time}_{cohort}.csv"), row.names = FALSE)
print("Subdistribution hazards model saved as .csv file")