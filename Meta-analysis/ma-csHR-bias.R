### "UDMI IPDMA" csHR Meta-Analysis with Bias assessment
### Author: Andrew Chapman
### Date: 2025-07-10

cohorts_to_include <- c("High-STEACS","APACE", "BACC", "KAROLINSKA", "ACTION")  # Optional: set to NULL to include all

# Define terms to iterate
times <- c("1y", "5y")
outcomes <- c("MACE", "Non_CV_Death", "HF")
models <- c("adjusted_pri_model", "adjusted_sec_model", "unadjusted")

# Term labels for plots
term_labels <- c(
        "udmi4Type 1 MI" = "Type 1 MI",
        "udmi4Type 2 MI" = "Type 2 MI",
        "udmi4Acute myocardial injury" = "Acute myocardial injury",
        "udmi4Chronic myocardial injury" = "Chronic myocardial injury"
)

# Optional safety check
if (!exists("cohorts_to_include")) cohorts_to_include <- NULL

# Initialize master list of all RMA models
res_list_named <- list()

# Loop through all combinations
for (time in times) {
        for (outcome in outcomes) {
                for (model in models) {
                        
                        file_path <- glue("Tables/cs/{time}/combined_csHR_{outcome}_{time}_{model}.csv")
                        
                        if (!file.exists(file_path)) {
                                message(glue("⚠️ File not found: {file_path} — skipping."))
                                next
                        }
                        
                        print(glue("📄 Processing: {file_path}"))
                        
                        df_pool <- read.csv(file_path)
                        
                        if (!all(c("term", "cohort", "estimate", "std.error") %in% names(df_pool))) {
                                message("❌ Missing required columns — skipping.")
                                next
                        }
                        
                        meta_rows <- list()
                        
                        for (term_value in unique(df_pool$term)) {
                                
                                subset_df <- df_pool %>%
                                        filter(term == term_value)
                                
                                # ✅ Only apply filter if cohorts_to_include is not NULL
                                if (!is.null(cohorts_to_include)) {
                                        subset_df <- subset_df %>%
                                                filter(cohort %in% cohorts_to_include)
                                }
                                
                                if (nrow(subset_df) < 2) {
                                        message(glue("⚠️ Not enough data for {term_value} — skipping."))
                                        next
                                }
                                
                                label <- term_labels[[term_value]]
                                model_key <- glue("{label} {outcome} {time}")
                                
                                res <- tryCatch({
                                        rma(yi = log(estimate), sei = std.error, data = subset_df, method = "REML", slab = cohort)
                                }, error = function(e) {
                                        message(glue("❌ Error for {model_key}: {e$message}"))
                                        return(NULL)
                                })
                                
                                if (!is.null(res)) {
                                        res_list_named[[model_key]] <- res
                                        
                                        # Extract meta result row for output
                                        meta_row <- extract_model_data(res, subset_df, outcome_label = label)
                                        meta_rows[[length(meta_rows) + 1]] <- meta_row
                                }
                        }
                        
                        # Combine and save meta-estimate result
                        if (length(meta_rows) > 0) {
                                result_df <- dplyr::bind_rows(meta_rows)
                                var_name <- glue("bias_combined_cshr_{time}_{outcome}_{model}")
                                
                                # Save to global environment
                                assign(var_name, result_df, envir = .GlobalEnv)
                                
                                # Save to file
                                out_file <- glue("Tables/cs/{time}/bias_meta_estimate_csHR_{outcome}_{time}_{model}.csv")
                                write.csv(result_df, out_file, row.names = FALSE)
                                print(glue("✅ Saved meta-estimate: {out_file}"))
                        }
                }
        }
}

# Store master RMA model list
assign("res_list_named", res_list_named, envir = .GlobalEnv)
