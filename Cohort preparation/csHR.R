# Code for running cause-specific Cox model
# Output produces: 
# -- Forest Plots and Tables for csHR
# -- subgroup analysis by interaction terms (table)
# -- T2MI predictors (table)

# Cause-specific Cox model  -----------------------------------------------

## Main analysis----

run_cox_models(df = df, time = time, status = status, status_code = 1, model_name_prefix = glue("MACE_{cohort}"), save.fig = TRUE)
run_cox_models(df = df, time = time, status = status, status_code = 2, model_name_prefix = glue("Non_CV_Death_{cohort}"), save.fig = TRUE)


## Subgroup analysis by interaction terms ----------------------------------

# Fit cause-specific Cox model 
if (fu_time == "1y") {
  S <- Surv(df[[time]], df[[status]] == 1)
} else if (fu_time == "5y") {
  S <- Surv(df[[time]], df[[status]] == 1)
  #S <- with(df, Surv(FUTagebisAMI_ohneIndex_Tod1825d, FUAMI_ohneIndex_Tod_CV1825d))
} 

cox_model <- coxph(S ~ 
                     udmi4 * age +
                     udmi4 * sex +
                     udmi4 * gfr +
                     udmi4 * PrevIHD +
                     udmi4 * DM,
                   data = df)

# Extract summary table as a tidy data frame
tidy_cox <- broom::tidy(cox_model, exponentiate = TRUE, conf.int = TRUE)

# Define your preferred order
ordered_patterns <- c(
  "udmi4Type 1 MI", 
  "udmi4Type 2 MI", 
  "udmi4Acute myocardial injury", 
  "udmi4Chronic myocardial injury"
)

# Add group identifier and arrange
tidy_cox_ordered <- tidy_cox %>%
  mutate(group = get_group(term)) %>%
  filter(!is.na(group)) %>%
  arrange(group, term) %>%
  select(term, estimate, conf.low, conf.high, p.value) %>%
  mutate(
    `exp(coef)` = estimate,
    `p < 0.05` = ifelse(p.value < 0.05, TRUE, FALSE)
  ) %>%
  select(term, `exp(coef)`, conf.low, conf.high, p.value, `p < 0.05`)

# View the reordered output
tidy_cox_ordered

#save
write.csv(tidy_cox_ordered, glue("Tables/cs/{fu_time}/interaction_terms_fu_{fu_time}_{cohort}.csv"), row.names = FALSE)
print("Interaction terms model saved as .csv file")

## T2MI exploring predictive factors ---------------------------------------

df2 <- subset(df, udmi4 == "Type 2 MI")

if (fu_time == "1y") {
  S <- Surv(df2[[time]], df2[[status]] == 1)
} else if (fu_time == "5y") {
  S <- Surv(df2[[time]], df2[[status]] == 1)
} 

cox_T2 <- coxph(S ~ 
                  age +
                  sex +
                  gfr +
                  ischemic_ecg +
                  PrevIHD,
                data = df2)

summary(cox_T2)

# Extract summary table as a tidy data frame
tidy_cox_t2 <- broom::tidy(cox_T2, exponentiate = TRUE, conf.int = TRUE)

# Add group identifier and arrange
tidy_cox_ordered_t2 <- tidy_cox_t2 %>%
  mutate(group = get_group(term)) %>%
  select(term, estimate, conf.low, conf.high, p.value) %>%
  mutate(
    `exp(coef)` = estimate,
    `p < 0.05` = ifelse(p.value < 0.05, TRUE, FALSE)
  ) %>%
  select(term, `exp(coef)`, conf.low, conf.high, p.value, `p < 0.05`)

# save
write.csv(tidy_cox_ordered_t2, glue("Tables/cs/{fu_time}/subgroupT2_fu_{fu_time}_{cohort}.csv"), row.names = FALSE)
print("T2 predictors model saved as .csv file")
