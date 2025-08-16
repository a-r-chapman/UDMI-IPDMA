### "UDMI IPDMA" master file
### author: "Andrew Chapman"
### date: "2025-04-09"

source("Code/ma-library.R")
source("Code/ma-functions.R")

### csHR
source("Code/ma-data_prep-cs.R")
source("Code/ma-csHR.R")

### sHR
# select outcome and time to extract data
source("Code/ma-data-prep-sdh.R")

# select outcome and time for meta-estimates
source("Code/ma-sHR.R")

# forest plots for primary and secondary outcomes
source("Code/ma-forest_plot.R")

# risk of bias assessment
source("Code/ma-eggers.R")

### sensitivity analysis
# run in cohorts with injury only 
source("Code/ma-data_prep-cs-sa.R")
source("Code/ma-csHR-sa.R")
source("Code/ma-data-prep-sdh-sa.R")
source("Code/ma-sHR-sa.R")
source("Code/ma-forest_plot-sa.R")

# select low bias cohorts only
source("Code/ma-csHR-bias.R")
source("Code/ma-sHR-bias.R")
source("Code/ma-bias-forestplot.R")

# total number of patients
source("Code/eventsummary_all.R")

# interaction terms
source("Code/ma-interaction.terms.R")

