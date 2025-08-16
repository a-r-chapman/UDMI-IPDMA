# --- Preparation ---
setwd("/Users/Andrew/Documents/Work Documents/Postgraduate/Research/Current Papers/UDMI IPDMA/Cohort preparation")
library(dplyr)
library(tools)
library(glue)
library(metafor)

# Create environment to temporarily store loaded data
temp_env <- new.env()
loaded_dfs <- c()
study_folders <- list.dirs(full.names = TRUE, recursive = FALSE)

# Load all matching subgroupT2_fu_1y files
for (study_path in study_folders) {
        study_name <- basename(study_path)
        tables_folders <- list.dirs(study_path, recursive = TRUE, full.names = TRUE)
        tables_folders <- tables_folders[grepl("Tables$", tables_folders)]
        
        for (tables_path in tables_folders) {
                files <- list.files(tables_path, full.names = TRUE, recursive = TRUE)
                files <- files[!file.info(files)$isdir]
                
                for (file in files) {
                        if (!grepl("subgroupT2_fu_1y_.*\\.csv$", file, ignore.case = TRUE)) next
                        
                        file_name <- file_path_sans_ext(basename(file))
                        
                        # Exclude files from cohorts BACC, Olmsted, ACTION early
                        if (grepl("BACC|Olmsted", file_name, ignore.case = TRUE)) {
                                message(glue("⏭️ Skipping file due to excluded cohort: {file_name}"))
                                next
                        }
                        
                        df_name <- make.names(paste(study_name, file_name, sep = "_"))
                        
                        tryCatch({
                                df <- read.csv(file, fileEncoding = "UTF-8")
                                df <- cbind(file_name = file_name, df)
                                assign(df_name, df, envir = temp_env)
                                loaded_dfs[df_name] <- file
                        }, error = function(e) {
                                message(sprintf("❌ Error loading %s: %s", file, e$message))
                        })
                }
        }
}

# Extract and organize rows by term
term_rows_map <- list()

for (df_name in names(loaded_dfs)) {
        df <- get(df_name, envir = temp_env)
        
        if (!all(c("term", "exp.coef.", "conf.low", "conf.high") %in% names(df))) {
                message(glue("⚠️ Skipping {df_name} — missing expected columns"))
                next
        }
        
        file_name <- unique(df$file_name)[1]
        cohort <- sub(".*subgroupT2_fu_1y_([^_]+).*", "\\1", file_name)
        
        if (grepl("High-STEACS", file_name)) {
                cohort <- "High-STEACS"
        }
        
        df$cohort <- cohort
        
        for (i in seq_len(nrow(df))) {
                row <- df[i, ]
                
                # Extract and validate term
                term <- as.character(row$term)
                if (term == "" || is.na(term)) next
                
                # Build row-specific data frame with correct types
                row_df <- data.frame(
                        term = term,
                        exp.coef. = as.numeric(row$exp.coef.),
                        conf.low = as.numeric(row$conf.low),
                        conf.high = as.numeric(row$conf.high),
                        cohort = cohort,
                        stringsAsFactors = FALSE
                )
                
                # Store row by term
                if (!term %in% names(term_rows_map)) {
                        term_rows_map[[term]] <- list()
                }
                term_rows_map[[term]][[length(term_rows_map[[term]]) + 1]] <- row_df
        }
}

# Meta-analysis
meta_summary_list <- list()
rma_models_list <- list()

for (term in names(term_rows_map)) {
        term_data <- bind_rows(term_rows_map[[term]])
        
        # Exclude specific cohorts again as a safety net
        term_data <- term_data %>% filter(!cohort %in% c("Olmsted", "BACC"))
        
        if (nrow(term_data) < 2) {
                message(glue("⚠️ Skipping {term} — fewer than 2 cohorts after exclusion"))
                next
        }
        
        # Compute estimate and std.error
        term_data <- term_data %>%
                mutate(
                        estimate = log(exp.coef.),
                        std.error = (log(conf.high) - log(conf.low)) / (2 * 1.96)
                )
        
        # Meta-analysis
        res <- tryCatch({
                rma(yi = estimate, sei = std.error, data = term_data, method = "REML", slab = cohort)
        }, error = function(e) {
                message(glue("❌ Error for {term}: {e$message}"))
                return(NULL)
        })
        
        if (is.null(res)) next
        
        model_key <- make.names(term)
        rma_models_list[[model_key]] <- res
        
        meta_summary_list[[length(meta_summary_list) + 1]] <- data.frame(
                term = term,
                HR = round(exp(res$beta), 2),
                CI_lower = round(exp(res$ci.lb), 2),
                CI_upper = round(exp(res$ci.ub), 2),
                p_value = signif(res$pval, 3),
                I2 = round(res$I2, 1),
                tau2 = round(res$tau2, 4),
                k = res$k,
                stringsAsFactors = FALSE
        )
        
        # Save combined data to global environment
        assign(paste0("combined_", make.names(term)), term_data, envir = .GlobalEnv)
        message(glue("✅ Created object: combined_{make.names(term)}"))
}

# Combine and export summary
meta_summary_subgroupT2 <- bind_rows(meta_summary_list)
assign("meta_summary_subgroupT2", meta_summary_subgroupT2, envir = .GlobalEnv)
assign("rma_models_subgroupT2", rma_models_list, envir = .GlobalEnv)

# Optional preview
print(meta_summary_subgroupT2, row.names = FALSE)
#write.csv(meta_summary_subgroupT2, file="subgroup_t2.csv")

term_data <- NULL

# Reset tracking for 5-year analysis
temp_env <- new.env()
loaded_dfs <- c()
term_rows_map <- list()


# Load all matching subgroupT2_fu_5y files
for (study_path in study_folders) {
        study_name <- basename(study_path)
        tables_folders <- list.dirs(study_path, recursive = TRUE, full.names = TRUE)
        tables_folders <- tables_folders[grepl("Tables$", tables_folders)]
        
        for (tables_path in tables_folders) {
                files <- list.files(tables_path, full.names = TRUE, recursive = TRUE)
                files <- files[!file.info(files)$isdir]
                
                for (file in files) {
                        if (!grepl("subgroupT2_fu_5y_.*\\.csv$", file, ignore.case = TRUE)) next
                        
                        file_name <- file_path_sans_ext(basename(file))
                        
                        # Exclude files from cohorts BACC, Olmsted, ACTION early
                        if (grepl("BACC|Olmsted", file_name, ignore.case = TRUE)) {
                                message(glue("⏭️ Skipping file due to excluded cohort: {file_name}"))
                                next
                        }
                        
                        df_name <- make.names(paste(study_name, file_name, sep = "_"))
                        
                        tryCatch({
                                df <- read.csv(file, fileEncoding = "UTF-8")
                                df <- cbind(file_name = file_name, df)
                                assign(df_name, df, envir = temp_env)
                                loaded_dfs[df_name] <- file
                        }, error = function(e) {
                                message(sprintf("❌ Error loading %s: %s", file, e$message))
                        })
                }
        }
}

# Extract and organize rows by term
term_rows_map <- list()

for (df_name in names(loaded_dfs)) {
        df <- get(df_name, envir = temp_env)
        
        if (!all(c("term", "exp.coef.", "conf.low", "conf.high") %in% names(df))) {
                message(glue("⚠️ Skipping {df_name} — missing expected columns"))
                next
        }
        
        file_name <- unique(df$file_name)[1]
        cohort <- sub(".*subgroupT2_fu_5y_([^_]+).*", "\\1", file_name)
        
        if (grepl("High-STEACS", file_name)) {
                cohort <- "High-STEACS"
        }
        
        df$cohort <- cohort
        
        for (i in seq_len(nrow(df))) {
                row <- df[i, ]
                
                # Extract and validate term
                term <- as.character(row$term)
                if (term == "" || is.na(term)) next
                
                # Build row-specific data frame with correct types
                row_df <- data.frame(
                        term = term,
                        exp.coef. = as.numeric(row$exp.coef.),
                        conf.low = as.numeric(row$conf.low),
                        conf.high = as.numeric(row$conf.high),
                        cohort = cohort,
                        stringsAsFactors = FALSE
                )
                
                # Store row by term
                if (!term %in% names(term_rows_map)) {
                        term_rows_map[[term]] <- list()
                }
                term_rows_map[[term]][[length(term_rows_map[[term]]) + 1]] <- row_df
        }
}

# Meta-analysis
meta_summary_list <- list()
rma_models_list <- list()

for (term in names(term_rows_map)) {
        term_data <- bind_rows(term_rows_map[[term]])
        
        # Exclude specific cohorts again as a safety net
        term_data <- term_data %>% filter(!cohort %in% c("Olmsted", "BACC"))
        
        if (nrow(term_data) < 2) {
                message(glue("⚠️ Skipping {term} — fewer than 2 cohorts after exclusion"))
                next
        }
        
        # Compute estimate and std.error
        term_data <- term_data %>%
                mutate(
                        estimate = log(exp.coef.),
                        std.error = (log(conf.high) - log(conf.low)) / (2 * 1.96)
                )
        
        # Meta-analysis
        res <- tryCatch({
                rma(yi = estimate, sei = std.error, data = term_data, method = "REML", slab = cohort)
        }, error = function(e) {
                message(glue("❌ Error for {term}: {e$message}"))
                return(NULL)
        })
        
        if (is.null(res)) next
        
        model_key <- make.names(term)
        rma_models_list[[model_key]] <- res
        
        meta_summary_list[[length(meta_summary_list) + 1]] <- data.frame(
                term = term,
                HR = round(exp(res$beta), 2),
                CI_lower = round(exp(res$ci.lb), 2),
                CI_upper = round(exp(res$ci.ub), 2),
                p_value = signif(res$pval, 3),
                I2 = round(res$I2, 1),
                tau2 = round(res$tau2, 4),
                k = res$k,
                stringsAsFactors = FALSE
        )
        
        # Save combined data to global environment
        assign(paste0("combined_", make.names(term)), term_data, envir = .GlobalEnv)
        message(glue("✅ Created object: combined_{make.names(term)}"))
}

# Combine and export summary
meta_summary_subgroupT2 <- bind_rows(meta_summary_list)
assign("meta_summary_subgroupT2", meta_summary_subgroupT2, envir = .GlobalEnv)
assign("rma_models_subgroupT2", rma_models_list, envir = .GlobalEnv)

# Optional preview
print(meta_summary_subgroupT2, row.names = FALSE)
#write.csv(meta_summary_subgroupT2, file="subgroup_t2.csv")

