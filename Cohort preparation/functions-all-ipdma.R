#Functions

# Function Forest Plots ---------------------------------------------------

run_cox_models <- function(df, time, status, status_code, model_name_prefix, save.fig = T) {
    
  require(survival)
  require(survminer)
  require(ggplot2)
  require(glue)
  require(broom)
  
  stopifnot(is.numeric(df[[time]]))
  stopifnot(is.numeric(df[[status]]))
  
  #survival object
  surv_obj <- Surv(df[[time]], df[[status]] == status_code)
  
  #unadjusted
  cox_unadj <- coxph(surv_obj ~ udmi4, data = df, ties = "efron")
  
  
  #adjusted - central model for outcomes
  cox_adj_pri <- coxph(surv_obj ~ udmi4 + age + sex + gfr, data = df, ties = "efron")
  cox_adj_sec <- coxph(surv_obj ~ udmi4 + age + sex + gfr + PrevIHD + DM, data = df, ties = "efron")
  
  #extract exp(csHR) and 95%CI
  unadj_tidy <- tidy(cox_unadj, exponentiate = TRUE, conf.int = TRUE)
  adj_tidy_pri <- tidy(cox_adj_pri, exponentiate = TRUE, conf.int = TRUE)
  adj_tidy_sec <- tidy(cox_adj_sec, exponentiate = TRUE, conf.int = TRUE)
    
  #save as CSVs in working directory
  write.csv(unadj_tidy, glue("./Tables/cs/{fu_time}/{model_name_prefix}_status_{status_code}_unadjusted_fu_{fu_time}.csv"), row.names = FALSE)
  write.csv(adj_tidy_pri, glue("./Tables/cs/{fu_time}/{model_name_prefix}_status_{status_code}_adjusted_pri_model_fu_{fu_time}.csv"), row.names = FALSE)
  write.csv(adj_tidy_sec, glue("./Tables/cs/{fu_time}/{model_name_prefix}_status_{status_code}_adjusted_sec_model_fu_{fu_time}.csv"), row.names = FALSE)
  print("Unadjusted and adjusted model output saved as .csv files")
    
  # Create ggforest plots
  unadj_plot <- ggforest(cox_unadj, data = df, main = paste0(model_name_prefix, " - Status ", status_code, " (Unadjusted)"))
  adj_pri_plot <- ggforest(cox_adj_pri, data = df, main = paste0(model_name_prefix, " - Status ", status_code, glue(" (Adjusted - pri. model)")))
  adj_sec_plot <- ggforest(cox_adj_sec, data = df, main = paste0(model_name_prefix, " - Status ", status_code, glue(" (Adjusted - sec. model)")))
    
  # Save plots
  if (save.fig == TRUE) {
    ggsave(paste0(model_name_prefix, "_status", status_code, glue("_unadjusted_forest_fu_{fu_time}.png")), plot = unadj_plot, path = glue("Figures/{fu_time}"), width = 8, height = 6)
    ggsave(paste0(model_name_prefix, "_status", status_code, glue("_adjusted_pri_model_forest_fu_{fu_time}.png")), plot = adj_pri_plot, path = glue("Figures/{fu_time}"), width = 10, height = 8)
    ggsave(paste0(model_name_prefix, "_status", status_code, glue("_adjusted_sec_model_forest_fu_{fu_time}.png")), plot = adj_sec_plot, path = glue("Figures/{fu_time}"), width = 10, height = 8)
    print("Unadjusted and adjusted forest plots saved as .png files")
  } else (print("Forest plots were not saved"))
    
  # Return summary invisibly
  invisible(list(unadjusted = unadj_tidy, adjusted_pri = adj_tidy_pri, adjusted_sec = adj_tidy_sec, plots = list(unadjusted_plot = unadj_plot, adjusted_plot = c(adj_pri_plot, adj_sec_plot))))
}


# helper function to match patterns ---------------------------------------
get_group <- function(term) {
  match <- sapply(ordered_patterns, function(p) grepl(p, term))
  group <- apply(match, 1, function(row) if (any(row)) which(row)[1] else NA)
  return(group)
}


# Manual tidy for readable output -----------------------------------------

tidy_crr <- function(model) {
  est <- model$coef
  se <- sqrt(diag(model$var))
  z <- est / se
  p <- 2 * pnorm(-abs(z))
  lower <- est - 1.96 * se
  upper <- est + 1.96 * se
  
  data.frame(
    term = names(est),
    estimate = exp(est),
    std.error = se,
    conf.low = exp(lower),
    conf.high = exp(upper),
    p.value = p,
    stringsAsFactors = FALSE
  )
}
