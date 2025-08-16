# Project: ipdma T1 and T2 MI
# Code: prepare data
# 07 May 2025
# Pedro Lopez Ayala and Andrew Chapman
# Written in R version 4.2.3

version$version.string 

# Create folders ----------------------------------------------------------

dir.create("Code", showWarnings = FALSE)
dir.create("Tables", showWarnings = FALSE)
dir.create("Tables/cs", showWarnings = FALSE)
dir.create("Tables/cs/1y", showWarnings = FALSE)
dir.create("Tables/cs/5y", showWarnings = FALSE)
dir.create("Tables/sh", showWarnings = FALSE)
dir.create("Tables/sh/1y", showWarnings = FALSE)
dir.create("Tables/sh/5y", showWarnings = FALSE)
dir.create("Figures", showWarnings = FALSE)
dir.create("Figures/1y", showWarnings = FALSE)
dir.create("Figures/5y", showWarnings = FALSE)


# Load df ----------------------------------------------------------------

file <- "/Users/Andrew/Documents/Work Documents/Postgraduate/Research/Current Papers/UDMI IPDMA/Cohort preparation/Karolinska/fullswede.rds" #please insert here the directory to your masterfile
masterfile <- readRDS(file)
masterfile <- as.data.frame(masterfile)


# df management I ---------------------------------------------------------

## labelled variables to factor----
masterfile <- masterfile %>% mutate_if(is.labelled, as_factor) #in case your masterfile has labelled variables
df <- masterfile

df$T1MI <- ifelse(df$udmi4=="T1",1,0)
df$T2MI <- ifelse(df$udmi4=="T2",1,0)

## Rename variables----
# These are the key variables for model adjustment. Please do NOT change the name of the left hand side
df <- df %>%
        rename(age = Age_Index,  #Please substitute the right hand side var name for the name of your variable
               sex = Sex, #Please substitute the right hand side var name for the name of your variable
               PrevIHD = PrevIHD, #Please substitute the right hand side var name for the name of your variable
               DM = Diabetes_d, #Please substitute the right hand side var name for the name of your variable
               gfr = gfr, #Please substitute the right hand side var name for the name of your variable
               ischemic_ecg = `ECG STT-deviation (only MI patients)`,
               T1MI = T1MI, #T1MI as final diagnosis during index event
               T2MI = T2MI #T2MI as final diagnosis during index event
        )

## Check class----
sapply(df %>% select(
  age,
  sex,
  PrevIHD,
  DM,
  gfr,
  T1MI,
  T2MI
), function(x) class(x))

df$T1MI <- as.logical(df$T1MI)
df$T2MI <- as.logical(df$T2MI)

stopifnot(is.logical(df$T1MI)) # It is important that T1MI is as logical as this allows using either 1 or TRUE (for code below)
stopifnot(is.logical(df$T2MI)) # It is important that T2MI is as logical as this allows using either 1 or TRUE (for code below)


# Select Population -------------------------------------------------------

## mutate variables----
# Need to create a binary variable indicating whether Tn is missing
df <- df %>% 
        mutate(peakmax = do.call(pmax, c(select(., starts_with("trop_peak_24t_max")), na.rm = TRUE)),
               tn_missing = ifelse(is.na(peakmax), 1, 0))

table(df$tn_missing, useNA = 'ifany')

# Remove patients according to the exclusion criteria of your cohort
docl <- list()
docl[["Total_Pop"]] <- nrow(df)
df <- df %>% filter(tn_missing == 0) #remove patients without Tn
docl[["Excluded_tn_missing"]] <- docl$Total_Pop - nrow(df)
docl[["analysis_pop"]] <- nrow(df)
docl

# df management II -----------------------------------------------------------

## mutate variables----

### udmi var----
# The models require a variable called udmi4 (including Type 1 MI, Type 2 MI, Acute Injury, Chronic Injury, No Injury)
# this is the index diagnostic classification as per the UDMI
# If you already have a udmi4 variable please re-name here as below

udmi4_available <- TRUE #if you already have a udmi4 variable change to TRUE
if (udmi4_available == TRUE) {
  print("Rename your variable to udmi4")
  df$udmi4 <- as.character(df$udmi4)
  df$udmi4[df$udmi4 == "AMI"] <- "Acute myocardial injury"
  df$udmi4[df$udmi4 == "CMI"] <- "Chronic myocardial injury"
  df$udmi4[df$udmi4 == "T1"] <- "Type 1 MI"
  df$udmi4[df$udmi4 == "T2"] <- "Type 2 MI"
  df$udmi4[df$udmi4 == "xno myocardial injury"] <- "No myocardial injury"
  df$udmi4 <- factor(df$udmi4)
}

# If you have not subclassified acute or chronic injury, the code below will do this based on a 20% change in troponin where the diagosis is not T1 or T2

# To define this variable you need to create:
# -- peakmax defined as maximum Tn value
# -- peakmin defined as minimum Tn value
# -- ratio 
# -- acute defined as ratio > 0.2 and peak Tn > assay-specific URL
# -- acute injury, defined as not T1MI, not T2MI and acute injury == 1

if (udmi4_available == FALSE) {
  assay_url <- 14 #insert the 99th percentile (URL) for the assay used
  df <- df %>% 
    mutate(peakmax = do.call(pmax, c(select(., starts_with("RochehsTnT_combi_correct_")), na.rm = TRUE)),
           peakmin = do.call(pmin, c(select(., starts_with("RochehsTnT_combi_correct_")), na.rm = TRUE)),
           ratio = abs((peakmax - peakmin) / peakmin),
           acute = ifelse(ratio > 0.2 & peakmax > assay_url, 1, 0),
           acute.injury = ifelse(T1MI == FALSE & T2MI == FALSE & acute == 1, 1, 0),
           chronic.injury = ifelse(ratio <= 0.2 & T1MI == FALSE & T2MI == FALSE & peakmax > assay_url, 1, 0),
           udmi4 = case_when(T1MI == TRUE ~ "Type 1 MI",
                             T2MI == TRUE ~ "Type 2 MI", 
                             acute.injury == 1 ~ "Acute myocardial injury",
                             chronic.injury == 1 ~ "Chronic myocardial injury",
                             .default = "No myocardial injury")
    )
  class(df$udmi4)
  df$udmi4 <- factor(df$udmi4)
  
}

### reorder factor level for reference group----
# If your cohort ONLY includes patients with injury - (ie OLMSTED-Gulati, NY HARBOR-Smilowitz, Long term-Chapman) please adjust so chronic injury is the reference
# to do this, remove "no myocardial injury" and place "chronic myocardial injury" first in list  
# this is important for the regression model output
# In your cohort includes both patients with and without injury - (e.g. Edinburgh, Karolinska, APACE) you will find at the end of this file code for a sensitivity analysis where patients w/o myocardial injury are removed

only_injury <- FALSE #change to TRUE if you only have patients with injury
if (only_injury == TRUE) {
  df$udmi4 <- factor(df$udmi4, levels = c("Chronic myocardial injury", #first level our reference level
                                          "Type 1 MI", 
                                          "Type 2 MI", 
                                          "Acute myocardial injury"))
} else if (only_injury == FALSE) {
  df$udmi4 <- factor(df$udmi4, levels = c("No myocardial injury", #first level our reference level
                                          "Type 1 MI", 
                                          "Type 2 MI", 
                                          "Acute myocardial injury",
                                          "Chronic myocardial injury"))
}

print(table(df$udmi4, useNA = 'ifany'))
print(prop.table(table(df$udmi4, useNA = 'ifany')))

# Code for FU Variables ------------------------------------------------------

## dictionary----
# dead = all cause death during follow up
# death.30 = death up to 30d [0 or 1]
# death.1y = death up to 1y [0 or 1]
# CV_death = CV death during follow up
# CV.death.30 = CV death up to 30d [0 or 1]
# CV.death.1y = CV death up to 1y [0 or 1]
# AMI = MI during follow up (not including index AMI)
# AMI.30 = MI up to 30d [0 or 1] (not including index AMI)
# AMI.1y = MI up to 1y [0 or 1] (not including index AMI)
# MI.or.CVD = MI or CVD during follow up (not including index AMI)
# MI.or.CVD.30 = MI or CVD up to 30d [0 or 1] (not including index AMI)
# MI.or.CVD.1y = MI or CVD up to 1y [0 or 1] (not including index AMI)
# days.to.mi.death.or.follow.up = your follow up variable days to event 
# Noncardiodeath = Non cardiovascular death during follow up
# NonCV.death.30 = Non-CV death up to 30d [0 or 1]
# NonCV.death.1y = Non-CV death up to 1y [0 or 1]
# days.to.mi.death.1y = follow up censored at 365
# days.to.mi.death.5y = follow up censored at 1826

## Code for renaming and creating new variables----
# Please substitute the right hand side var name (e.g line one = "dead") for the name of your variable. 
df <- df %>% mutate(dead = dead,
                    death.30 = ifelse(dead==1&daysdeath<=30, 1,0),
                    death.1y = ifelse(dead==1&daysdeath<=365.25, 1,0),
                    days.to.mi.death.or.follow.up = pmin(df$MI_days, df$CV_death_days),
                    CV_death = CV_death,
                    CV.death.30 = ifelse(CV_death==1&CV_death_days<=30, 1,0),
                    CV.death.1y = ifelse(CV_death==1&CV_death_days<=365.25, 1,0), 
                    AMI = ifelse(AMI==1&MI_days>=30,1,0),
                    AMI.30 = ifelse(AMI==1&MI_days<=30, 1,0),
                    AMI.1y = ifelse(AMI==1&MI_days<=365.25, 1,0),
                    MI.or.CVD =  ifelse(AMI==1|CV_death==1,1,0),
                    MI.or.CVD.30 = ifelse(MI.or.CVD==1&days.to.mi.death.or.follow.up<=30, 1,0),
                    MI.or.CVD.1y = ifelse(MI.or.CVD==1&days.to.mi.death.or.follow.up<=365.25, 1,0),
                    MI.or.CVD.5y = ifelse(MI.or.CVD==1&days.to.mi.death.or.follow.up<=1826.25, 1,0),
                    NonCV.death.30 = ifelse(noncardiodeath==1&noncardiodeath_days<=30, 1,0),
                    NonCV.death.1y = ifelse(noncardiodeath==1&noncardiodeath_days<=365.25, 1,0),
                    NonCV.death.5y = ifelse(noncardiodeath==1&noncardiodeath_days<=1826.25, 1,0),
                    days.to.mi.death.1y = pmin(MI_days, CV_death_days),
                    days.to.mi.death.1y = ifelse(days.to.mi.death.1y>365.25, 365.25, days.to.mi.death.1y),
                    days.to.mi.death.5y = pmin(MI_days, CV_death_days),
                    days.to.mi.death.5y = ifelse(days.to.mi.death.5y>1826.25, 1826.25, days.to.mi.death.5y),
)

## Competing risk variables----
# Competing risk variables (3 level class: 0 censored, 1 MI or CV death, 2 non CV death)
df <- df %>% mutate(status.1y = ifelse(MI.or.CVD.1y == 1, 1, 0),
                    status.1y = ifelse(NonCV.death.1y == 1 & MI.or.CVD.1y == 0, 2, status.1y),
                    status.5y = ifelse(MI.or.CVD.5y == 1, 1, 0),
                    status.5y = ifelse(NonCV.death.5y == 1 & MI.or.CVD.5y == 0, 2, status.5y)
)

print(table(df$status.1y, useNA = 'ifany')) #0 censored, 1 MI or CV death, 2 non CV death (competing event)
print(sum(is.na(df$days.to.mi.death.1y)))
print(table(df$status.5y, useNA = 'ifany')) #0 censored, 1 MI or CV death, 2 non CV death (competing event)
print(sum(is.na(df$days.to.mi.death.5y)))

print("If missings in time or status variable, please check before running analysis")

## Code for HF variable----
# HF event variable during follow-up
HF_available <- FALSE #please change to FALSE in case your cohort has no heart failure event data during follow-up
if (HF_available == TRUE) {
  source("Code/create-var-hf-ipdma.R")
  df <- df %>% mutate(HF = FUHF,
                      HF.30 = FUHF.30,
                      HF.1y = FUHF.1y,
                      HF.5y = FUHF.5y,
                      status.1y.sec = ifelse(HF.1y == 1, 1, 0),
                      status.1y.sec = ifelse(HF.1y == 0 & death.1y == 1, 2, status.1y.sec),
                      status.5y.sec = ifelse(HF.5y == 1, 1, 0),
                      status.5y.sec = ifelse(HF.5y == 0 & FUTod1825d == 1, 2, status.5y.sec),
                      days.to.hf.death.1y = pmin(days.to.hf.1y, FUTagebisTod365d, FUTagebisletzterFUKontakt),
                      days.to.hf.death.5y = pmin(days.to.hf.5y, FUTagebisTod1825d, FUTagebisletzterFUKontakt))
  sapply(df %>% select(HF, HF.30, HF.1y, HF.5y, status.1y.sec, status.5y.sec), function(x) table(x, useNA = 'ifany'))
}


# Subset data for sensitivity analysis ------------------------------------

# Some cohorts do not have "no myocardial injury" patients.
# To be able to pool all cohorts, we will need to remove "no myocardial injury" in those cohorts with such patients 

if (only_injury == FALSE) {
  df_sa <- df %>% filter(udmi4 != "No myocardial injury")
  df_sa$udmi4 <- factor(df_sa$udmi4, 
                        levels = c("Chronic myocardial injury", #first level our reference level
                                   "Type 1 MI", 
                                   "Type 2 MI", 
                                   "Acute myocardial injury"))
  table(df_sa$udmi4, useNA = 'ifany')
}


