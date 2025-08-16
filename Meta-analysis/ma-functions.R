### title: "Functions"
# author: "Andrew Chapman"
# date: "2025-04-09"


###
# Function to extract model data and include heterogeneity info
extract_model_data <- function(res, original_data, outcome_label) {
        
        # Dynamically find the 'event' column
        event_col <- grep("event", names(original_data), value = TRUE, ignore.case = TRUE)[1]
        if (is.na(event_col)) {
                stop("No column containing 'event' found in original_data.")
        }
        
        # Rename for consistency
        original_data <- original_data %>%
                rename(event = all_of(event_col))
        
        # Study-level data (log scale)
        study_df <- data.frame(
                study = as.character(res$slab),
                estimate = res$yi,               # log HR per study
                se = sqrt(res$vi),
                weight = weights(res),
                type = "Study",
                outcome = outcome_label
        ) %>%
                # Exponentiate estimates and calculate CIs on HR scale
                mutate(
                        HR = exp(estimate),
                        conf.low = exp(estimate - 1.96 * se),
                        conf.high = exp(estimate + 1.96 * se)
                ) %>%
                left_join(
                        original_data %>%
                                select(study = cohort, event, n, HR_orig = estimate, conf.low_orig = conf.low, conf.high_orig = conf.high) %>%
                                mutate(
                                        P_label = paste0(event, "/", n, " (", sprintf("%.1f", 100 * event / n), "%)"),
                                        HR_label = sprintf("%.2f [%.2f–%.2f]", HR_orig, conf.low_orig, conf.high_orig)
                                ),
                        by = "study"
                )
        
        # Format p-value helper
        format_pvalue <- function(p) {
                if (p < 0.001) {
                        return("< 0.001")
                } else {
                        return(sprintf("= %.3f", p))
                }
        }
        
        #construct heterogeneity string
        if (is.null(res$I2) || is.na(res$I2)) {
                heterogeneity_text <- "Heterogeneity: Not available"
        } else {
                p_formatted <- tryCatch(format_pvalue(res$QEp), error = function(e) "NA")
                heterogeneity_text <- sprintf(
                        "Heterogeneity: I² = %.0f%%, τ² = %.3f, Q = %.2f (df = %d), p %s",
                        res$I2, res$tau2, res$QE, res$k - res$p, p_formatted
                )
        }
        
        
        summary_df <- data.frame(
                study = "Meta-estimate",
                estimate = res$b[1],             # meta-estimate log HR
                se = res$se,
                weight = NA,
                type = "Summary",
                outcome = outcome_label,
                event = sum(original_data$event),
                n = sum(original_data$n),
                conf.low = res$ci.lb,
                conf.high = res$ci.ub
        ) %>%
                mutate(
                        HR = exp(estimate),
                        conf.low = exp(conf.low),
                        conf.high = exp(conf.high),
                        P_label = paste0(event, "/", n, " (", sprintf("%.1f", 100 * event / n), "%)"),
                        HR_label = sprintf("%.2f [%.2f–%.2f]", HR, conf.low, conf.high),
                        heterogeneity = heterogeneity_text
                )
        
        bind_rows(study_df, summary_df)
}



        