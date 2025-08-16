# For those cohorts with follow-up Heart Failure data


# cause-specific HR -------------------------------------------------------

if (fu_time == "1y") {
  time <- "days.to.hf.death.1y" #introduce your 1y time variable (time to AMI or CV death)
  status <- "status.1y.sec" #introduce your 1y status variable (three level variable: 0 censored, 1 MI or CV death, 2 non CV death [competing event])
} else if (fu_time == "5y") {
  time <- "days.to.hf.death.5y" #introduce your 5y time variable (time to AMI or CV death)
  status <- "status.5y.sec" #introduce your 5y status variable (three level variabel: 0 censored, 1 MI or CV death, 2 non CV death [competing event])
}

run_cox_models(df = df, time = time, status = status, status_code = 1, model_name_prefix = glue("HF_{cohort}"), save.fig = FALSE)
run_cox_models(df = df, time = time, status = status, status_code = 2, model_name_prefix = glue("All_cause_Death_{cohort}"), save.fig = FALSE)

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
fg_hf_adj <- crr(ftime = ftime_sh, fstatus = fstatus_sh, cov1 = adj_mat_sh, failcode = 1, cencode = 0)

### Non-CV death model (status == 2)----
fg_acd_adj <- crr(ftime = ftime_sh, fstatus = fstatus_sh, cov1 = adj_mat_sh, failcode = 2, cencode = 0)

## Results----
tidy_crr(fg_hf_adj)
tidy_crr(fg_acd_adj)

## Save----
write.csv(tidy_crr(fg_hf_adj), file = glue("Tables/sh/{fu_time}/fg_hf_adj_{model}_model_fu_{fu_time}_{cohort}.csv"), row.names = FALSE)
write.csv(tidy_crr(fg_acd_adj), file = glue("Tables/sh/{fu_time}/fg_acd_adj_{model}_model_fu_{fu_time}_{cohort}.csv"), row.names = FALSE)
print("Subdistribution hazards model saved as .csv file")