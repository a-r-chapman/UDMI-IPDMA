# Project: ipdma T1 and T2 MI
# Code: Table 1 
# 07 May 2025
# Pedro Lopez Ayala
# Written in R version 4.2.3

# Create Folders ----------------------------------------------------------

dir.create("Tables/Table1", showWarnings = F)

# Table one ---------------------------------------------------------------

## tableone----

library(tableone)
packageVersion('tableone') #‘0.13.2’
library(flextable)
packageVersion('flextable') #‘0.9.1’
library(officer)
packageVersion('officer') #‘0.6.2’

### Define variables ----
myVars_bl <- c( "age", "sex", "early_presenter", "Time_CPO", 
                "HTA", "DL", "DM", "Smoking", 
                "PrevIHD", "Prev_AMI", "PrevPCI_CABG", "Prev_stroke", 
                "stdep", "twave", "LBBB", "RBBB",
                "gfr",
                "antiplatelet_therapy", "anticoagulant_therapy",  
                "betablocker_therapy", "ACEIs_ARBs", "statin_therapy", "calciumchannel_therapy",
                "gtn_therapy")
catVars_bl <- c("sex", "early_presenter", 
                "HTA", "DL", "DM", "Smoking", 
                "PrevIHD", "Prev_AMI", "PrevPCI_CABG", "Prev_stroke", 
                "stdep", "twave", "LBBB", "RBBB",
                "antiplatelet_therapy", "anticoagulant_therapy", 
                "betablocker_therapy", "ACEIs_ARBs", "statin_therapy", "calciumchannel_therapy",
                "gtn_therapy") 
nnVars_bl <- c("age", "Time_CPO", "gfr")

### Create table one---- 
table_bl <- CreateTableOne(vars = myVars_bl, factorVars = catVars_bl, data = df)
table_bl

### Print TableOne---- 
tab_bl <- print(table_bl, noSpaces = T, nonnormal = nnVars_bl, catDigits = 1, contDigits = 1, quote = F) 
tab_bl <- tibble::rownames_to_column(as.data.frame(tab_bl), var = "Variable")

### Modify----
tab_bl$Variable <- gsub("(median [IQR])", "", fixed = T, tab_bl$Variable)
tab_bl$Variable <- gsub("= Yes (%)", "", fixed = T, tab_bl$Variable) 
tab_bl$Variable <- gsub("= TRUE (%)", "", fixed = T, tab_bl$Variable) 
tab_bl$Variable <- gsub("= 1 (%)", "", fixed = T, tab_bl$Variable) 
tab_bl$Variable <- gsub("(%)", "", fixed = T, tab_bl$Variable)
tab_bl$Variable <- gsub("age", "Age, y (median [IQR])", fixed = T, tab_bl$Variable)
tab_bl$Variable <- gsub("sex = female", "Female, n (%)", fixed = T, tab_bl$Variable)
tab_bl$Variable <- gsub("early_presenter", "Early Presenters (≤3h), n (%)", fixed = T, tab_bl$Variable)
tab_bl$Variable <- gsub("Time_CPO", "Time since CPO (median [IQR])", fixed = T, tab_bl$Variable)
tab_bl$Variable <- gsub("HTA", "Hypertension", fixed = T, tab_bl$Variable)
tab_bl$Variable <- gsub("DL", "Hypercholesterolemia", fixed = T, tab_bl$Variable)
tab_bl$Variable <- gsub("DM", "Diabetes mellitus", fixed = T, tab_bl$Variable)
tab_bl$Variable <- gsub("Smoking", "Current smoker", fixed = T, tab_bl$Variable)
tab_bl$Variable <- gsub("PrevIHD", "Known CAD", fixed = T, tab_bl$Variable)
tab_bl$Variable <- gsub("Prev_AMI", "Previous MI", fixed = T, tab_bl$Variable)
tab_bl$Variable <- gsub("PrevPCI_CABG", "Previous Revascularization", fixed = T, tab_bl$Variable)
tab_bl$Variable <- gsub("Prev_stroke", "Previous stroke", fixed = T, tab_bl$Variable)
tab_bl$Variable <- gsub("LBBB", "LBBB", fixed = T, tab_bl$Variable)
tab_bl$Variable <- gsub("RBBB", "RBBB", fixed = T, tab_bl$Variable)
tab_bl$Variable <- gsub("stdep", "ST-depression", fixed = T, tab_bl$Variable)
tab_bl$Variable <- gsub("twave", "T-wave inversion", fixed = T, tab_bl$Variable)
tab_bl$Variable <- gsub("gfr", "eGFR, ml/min/1.73m^3", fixed = T, tab_bl$Variable)
tab_bl$Variable <- gsub("AufnahmelaborHbgl", "Hemoglobin, g/dL", fixed = T, tab_bl$Variable)
tab_bl$Variable <- gsub("antiplatelet_therapy", "Antiplatelet therapy", fixed = T, tab_bl$Variable)
tab_bl$Variable <- gsub("anticoagulant_therapy", "Oral anticoagulation", fixed = T, tab_bl$Variable)
tab_bl$Variable <- gsub("betablocker_therapy", "ß-blocker", fixed = T, tab_bl$Variable)
tab_bl$Variable <- gsub("ACEIs_ARBs", "ACEIs/ARBs", fixed = T, tab_bl$Variable)
tab_bl$Variable <- gsub("statin_therapy", "Statin", fixed = T, tab_bl$Variable)
tab_bl$Variable <- gsub("calciumchannel_therapy", "Calcium antagonists", fixed = T, tab_bl$Variable)
tab_bl$Variable <- gsub("gtn_therapy", "Nitrates", fixed = T, tab_bl$Variable)

#View(tab_bl) #see final output

### Add header rows----
tab_bl_rows <- tab_bl %>% add_row(Variable = "CVRF, n (%)", Overall = "", .after = 5) %>% 
  add_row(Variable = "Medical History, n (%)", Overall = "", .after = 9 + 1) %>% 
  add_row(Variable = "ECG, n (%)", Overall = "", .after = 13 + 2) %>% 
  add_row(Variable = "Laboratory findings, median (IQR)", Overall = "", .after = 17 + 3) %>% 
  add_row(Variable = "Medications at presentation, n (%)", Overall = "", .after = 18 + 4)

#View(tab_bl_rows)

### Create flextable ----
ft_bl <- qflextable(tab_bl_rows) %>% autofit() %>% align(align = "left", part = "all") 
ft_bl <- bold(ft_bl, i = NULL, part = "header")
ft_bl <- bold(ft_bl, i = c(6, 11, 16, 21, 23))
ft_bl <- bg(ft_bl, i = NULL, bg = "wheat", part = "header")
ft_bl <- bg(ft_bl, i = c(6, 11, 16, 21, 23), bg = "#EFEFEF", part = "body")
ft_bl 

#### save----
##### html----
path = glue::glue("Tables/Table1/Table_one_ipdma_{cohort}.html")
save_as_html(ft_bl, path = path)

##### .docx----
my_doc <- officer::read_docx() %>% 
  flextable::body_add_flextable(ft_bl) %>% 
  officer::body_end_section_portrait() #body_end_section_landscape() for landscape orientation
target = glue::glue("Tables/Table1/Table_one_ipdma_{cohort}.docx")
print(my_doc, target = target)
