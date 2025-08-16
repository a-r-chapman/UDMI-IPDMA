# Main File
# For everything to work, you need to adapt the prepare-data-ipdma.R file. This file is key for preparing your dataset
# Flowchart-ipdma.R file should also be adapted according to your exclusion criteria
# table1-ipdma.R file needs to be adapted according to the variables you have in your dataset
# All other files do not need any adaptation

source("Code/function_check_packages.R")
using("dplyr", "haven", "glue", "broom", "survival", "survminer", "cmprsk", "riskRegression", "prodlim", 
      "ggplot2", "adjustedCurves", "patchwork", "scales", "consort", "tableone", "flextable", "officer")

# If all packages are installed, you can continue

source("Code/library-ipdma.R") #packages required
date <- Sys.Date() 
cohort <- glue("APACE-{date}") #type name of your cohort but leave "-{date}" (e.g. High-STEACS-{date}). Please no spaces in between
source("Code/prepare-data-ipdma.R") #code for preparing data
source("Code/flowchart-ipdma.R") #Flowchart following consort recommendations
source("Code/table1-ipdma.R") #Table 1 
source("Code/crude-events-table.R") #obtain table with crude events at 30d and 1y
source("Code/functions-all-ipdma.R") #functions needed for formal regression analysis. Source only once

# If you have 5 year follow up available, please re run scripts from line 23 below to 45 with "5y" rather than "1y" (default)
fu_time <- c("1y", "5y")[1] #define FU time (select 1y with [1] or 5y with [5])
if (fu_time == "1y") {
  time <- "days.to.mi.death.1y" #introduce your 1y time variable (time to AMI or CV death)
  status <- "status.1y" #introduce your 1y status variable (three level variable: 0 censored, 1 MI or CV death, 2 non CV death [competing event])
} else if (fu_time == "5y") {
  time <- "days.to.mi.death.5y" #introduce your 5y time variable (time to AMI or CV death)
  status <- "status.5y" #introduce your 5y status variable (three level variabel: 0 censored, 1 MI or CV death, 2 non CV death [competing event])
}
source("Code/csHR.R") #code for running cause-specific Cox model

# The code below is for sensitivity analysis removing the non myocardial injury group and running cause-specific Cox model
if (only_injury == FALSE) {
  run_cox_models(df = df_sa, time = time, status = status, status_code = 1, model_name_prefix = glue("MACE_sa_{cohort}"), save.fig = FALSE)
  run_cox_models(df = df_sa, time = time, status = status, status_code = 2, model_name_prefix = glue("Non_CV_Death_sa_{cohort}"), save.fig = FALSE)
} 

source("Code/cif.R") #code for obtaining CIF plots (unadjusted and adjusted)
model <- "primary" 
source("Code/sdHR.R") #code for running subdistribution (Fine and Gray) model
if (only_injury == FALSE) { source("Code/sdHR-sa.R")} #for sensitivity analysis removing non myocardial injury group and running subdistribution model
model <- "secondary"
source("Code/sdHR.R") #code for running subdistribution (Fine and Gray) model
if (only_injury == FALSE) { source("Code/sdHR-sa.R")} #for sensitivity analysis removing non myocardial injury group and running subdistribution model


# The code below is just for those cohorts with heart failure data during the follow-up
# Remember that the HF_available object is specified in the "prepare-data-ipdma.R" file 
# Run this code once you are done with the code above (1y and 5y)
# You can run this code independent of the above. Just need to have sourced:
# - "prepare-data-ipdma.R" file 
# - "functions-all-ipdma.R" file
# and define date and cohort objects

if (HF_available == TRUE) {
  fu_time <- c("1y", "5y")[1] #define FU time (select 1y with [1] or 5y with [5])
  model <- "primary" #For this secondary analysis we will only use the primary model
  source("Code/HF-analysis-ipdma.R") #time and event vars for HF endpoint are defined here
} else if (HF_available == FALSE) { print("you have no HF data in your cohort")}
