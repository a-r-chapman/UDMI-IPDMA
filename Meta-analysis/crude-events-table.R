# Code for crude events Table

out.table <- function(df, name) {
  
  require(glue)
  
  stopifnot(is.character(name))
  
  event_summary <- df %>%
    group_by(udmi4) %>%
    summarise(
      n = n(),
      across(all_of(event_vars), ~sum(.x == 1, na.rm = TRUE), .names = "{.col}_events"),
      .groups = "drop"
    ) %>%
    mutate(
      across(ends_with("_events") & !starts_with("n"),
             ~ .x / n, 
             .names = "{.col}_prop")
    ) %>%
    arrange(udmi4)
  
  
  print(event_summary)
  
  #create DF for each UDMI4
  assign(paste0(name, ".event.summary"), event_summary, envir = .GlobalEnv)
  
  #create CSV file for summary by subgroups 
  write.csv(event_summary, file = glue("./Tables/{name}_event_summary.csv"), row.names = FALSE)
  
  # Return both summaries invisibly
  invisible(event_summary)
}


# List of event variables defined in the prepare-data-ipdma.R file
if (fu_time == "1y") {
  if (HF_available == TRUE) {
    event_vars <- c(
      "AMI.30", "AMI.1y", "death.30", "death.1y", "MI.or.CVD.30", "MI.or.CVD.1y",
      "CV.death.30", "CV.death.1y", "NonCV.death.30", "NonCV.death.1y",
      "AMI", "CV_death", "MI.or.CVD", "noncardiodeath", "dead",
      "HF.30", "HF.1y", "HF.5y", "HF"
    )
  } else if (HF_available == FALSE) {
    event_vars <- c(
      "AMI.30", "AMI.1y", "death.30", "death.1y", "MI.or.CVD.30", "MI.or.CVD.1y",
      "CV.death.30", "CV.death.1y", "NonCV.death.30", "NonCV.death.1y",
      "AMI", "CV_death", "MI.or.CVD", "noncardiodeath", "dead"
    )
  }
} else if (fu_time == "5y") {
  if (HF_available == TRUE) {
    event_vars <- c(
      "AMI.30", "AMI.1y", "AMI.5y", "death.30", "death.1y", "death.5y", "MI.or.CVD.30", "MI.or.CVD.1y", "MI.or.CVD.5y",
      "CV.death.30", "CV.death.1y", "CV.death.5y", "NonCV.death.30", "NonCV.death.1y", "NonCV.death.5y",
      "AMI", "CV_death", "MI.or.CVD", "noncardiodeath", "dead",
      "HF.30", "HF.1y", "HF.5y", "HF"
    )
  } else if (HF_available == FALSE) {
    event_vars <- c(
      "AMI.30", "AMI.1y", "AMI.5y", "death.30", "death.1y", "death.5y", "MI.or.CVD.30", "MI.or.CVD.1y", "MI.or.CVD.5y",
      "CV.death.30", "CV.death.1y", "CV.death.5y", "NonCV.death.30", "NonCV.death.1y", "NonCV.death.5y",
      "AMI", "CV_death", "MI.or.CVD", "noncardiodeath", "dead"
    )
  }
}

# outcome table stratified by UDMI4
out.table(df, glue("{cohort}"))
