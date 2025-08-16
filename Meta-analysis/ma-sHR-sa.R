### "UDMI IPDMA" sHR Meta-Analysis
### Author: Andrew Chapman
### Date: 2025-07-10

# Define parameters
times <- c("1y", "5y")
outcomes <- c("mace", "ncv", "hf")
models <- c("adj_primary_model", "adj_secondary_model")

# Term labels for plot output
term_labels <- c(
        "udmi4Type 1 MI" = "Type 1 MI",
        "udmi4Type 2 MI" = "Type 2 MI",
        "udmi4Acute myocardial injury" = "Acute myocardial injury",
        "udmi4Chronic myocardial injury" = "Chronic myocardial injury"
)

# Loop through all combinations
for (time in times) {
        for (outcome in outcomes) {
                for (model in models) {
                        
                        # Define path to pooled sHR data
                        file_path <- glue("Tables/sh/{time}/combined_sHR_sa_{outcome}_{time}_{model}.csv")
                        
                        # Check file exists
                        if (!file.exists(file_path)) {
                                message(glue("⚠️ File not found: {file_path} — skipping."))
                                next
                        }
                        
                        print(glue("📄 Processing: {file_path}"))
                        
                        # Read pooled data
                        df_pool <- read.csv(file_path)
                        
                        # Check necessary columns
                        if (!all(c("term", "cohort", "estimate", "std.error") %in% names(df_pool))) {
                                message("❌ Missing required columns for meta-analysis — skipping.")
                                next
                        }
                        
                        # Create data frame names for each term
                        new_df_names <- c()
                        for (term_value in unique(df_pool$term)) {
                                subset_df <- df_pool %>% filter(term == term_value)
                                suffix <- sub(".*status_1", "status_1", subset_df$file_name[1])
                                df_name <- make.names(paste0(term_value, "_", suffix))
                                assign(df_name, subset_df, envir = .GlobalEnv)
                                new_df_names <- c(new_df_names, df_name)
                        }
                        
                        # Run meta-analysis for each term subset
                        res_list <- list()
                        for (i in seq_along(new_df_names)) {
                                df <- get(new_df_names[i])
                                res <- tryCatch({
                                        rma(yi = log(estimate), sei = std.error, data = df, method = "REML", slab = cohort)
                                }, error = function(e) {
                                        message(glue("❌ Error in meta-analysis for {new_df_names[i]}: {e$message}"))
                                        return(NULL)
                                })
                                res_list[[i]] <- res
                                assign(paste0("res_", outcome, "_", time, "_", model, "_", i), res, envir = .GlobalEnv)
                        }
                        
                        # Filter valid results
                        valid_indices <- which(!sapply(res_list, is.null))
                        res_list <- res_list[valid_indices]
                        new_df_names <- new_df_names[valid_indices]
                        
                        if (length(res_list) == 0) {
                                message(glue("⚠️ No valid meta-analysis results for: {file_path}"))
                                next
                        }
                        
                        # Extract and combine results
                        var_name <- glue("combined_shr_sa_{time}_{outcome}_{model}")
                        result_df <- purrr::map2_dfr(
                                .x = seq_along(res_list),
                                .y = new_df_names,
                                .f = function(i, df_name) {
                                        df <- get(df_name)
                                        term_value <- unique(df$term)
                                        label <- term_labels[[term_value]]
                                        extract_model_data(res_list[[i]], df, outcome_label = label)
                                }
                        )
                        
                        assign(var_name, result_df, envir = .GlobalEnv)
                        
                        # Save results to CSV
                        out_file <- glue("Tables/sh/{time}/meta_estimate_sHR_sa_{outcome}_{time}_{model}.csv")
                        write.csv(result_df, out_file, row.names = FALSE)
                        print(glue("✅ Saved meta-estimate to: {out_file}"))
                }
        }
}
