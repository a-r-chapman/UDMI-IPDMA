# Project: ipdma T1 and T2 MI
# Code: patient flowchart
# 07 May 2025
# Pedro Lopez Ayala and Andrew Chapman
# Written in R version 4.2.3

# Create Folders ----------------------------------------------------------

dir.create("Figures/Flowchart", showWarnings = FALSE)

# Patient Flowchart -------------------------------------------------------

time1 <- "April 2006" #time start of recruitment in your cohort
time2 <- "September 2020" #end of recruitment in your cohort

## text----
txt1 <- glue::glue("Patients recruited from \n {time1} to {time2} \n with at least one hs-cTnT available \n (n={docl$Total_Pop - docl$Excluded_tn_missing_0h})")
txt1_side <- glue::glue("Excluded (n={docl$Excluded_unclear + docl$Excluded_multiple_inclusions}):\n\u2022 Unclear diagnosis and hs-cTnT > URL (n={docl$Excluded_unclear}) \n\u2022 Multiple inclusions (n={docl$Excluded_multiple_inclusions})")
txt2 <- glue::glue("Available for meta-analysis \n (n={docl$analysis_pop})")

## flowchart----
if (packageVersion('consort') >= '1.1.0') { #since version 1.1.0 dist argument no longer works
  flow <- add_box(txt = txt1) %>% 
    add_side_box(txt = txt1_side) %>%   
    add_box(txt = txt2)  
}
if (packageVersion('consort') < '1.1.0') {
  flow <- add_box(txt = txt1) %>% 
    add_side_box(txt = txt1_side, dist = 0.05) %>%   
    add_box(txt = txt2, dist = 0.05) 
}

plot(flow)

### save----
png(glue::glue("Figures/Flowchart/consort_diagram_{cohort}.png"), 
    width = 24, 
    height = 14, 
    res = 300, 
    units = "cm", 
    type = "cairo") 
plot(flow)
dev.off() 

# Remove objects ----------------------------------------------------------

rm(txt1, txt1_side, txt2, flow)
