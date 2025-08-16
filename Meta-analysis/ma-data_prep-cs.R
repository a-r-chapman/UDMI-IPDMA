# Data preparation for cause specific HR endpoint
# Andrew Chapman and Pedro Lopez Ayala
# 15 June 2025
# R version 4.2.3

setwd("/Users/Andrew/Documents/Work Documents/Postgraduate/Research/Current Papers/UDMI IPDMA/Cohort preparation")

# --- Define helper function(s) here ---

bind_matching_dfs <- function(pattern, df_names, env) {
        matches <- grep(pattern, df_names, value = TRUE)
        matches <- matches[!grepl("_sa_|Edinburgh|Olmsted", matches)]
        matches <- matches[grepl(glue("{outcome}"), matches)]
        dfs <- lapply(matches, function(nm) get(nm, envir = env))
        bind_rows(dfs)
}


# Wrapper function variables -----------------------------------------------

times <- c("1y", "5y")
outcomes <- c("MACE", "Non_CV_Death", "HF")
models <- c("adjusted_pri_model", "adjusted_sec_model", "unadjusted")

# Create temp environment and load data -----------------------------------
temp_env <- new.env()
loaded_dfs <- c()
study_folders <- list.dirs(full.names = TRUE, recursive = FALSE)

for (study_path in study_folders) {
        study_name <- basename(dirname(study_path))
        tables_folders <- list.dirs(study_path, recursive = TRUE, full.names = TRUE)
        tables_folders <- tables_folders[grepl("Tables$", tables_folders)]
        
        for (tables_path in tables_folders) {
                files <- list.files(tables_path, full.names = TRUE, recursive = TRUE)
                files <- files[!file.info(files)$isdir]
                
                for (file in files) {
                        if (!grepl("\\.csv$", file, ignore.case = TRUE)) next
                        
                        file_name <- tools::file_path_sans_ext(basename(file))
                        df_name <- make.names(paste(study_name, file_name, sep = "_"))
                        
                        tryCatch({
                                df <- read.csv(file, fileEncoding = "UTF-8")
                                df <- cbind(file_name = file_name, df)
                                assign(df_name, df, envir = temp_env)
                                loaded_dfs[df_name] <- file
                                rm(df)
                        }, error = function(e) {
                                message(sprintf("❌ Error loading %s: %s", file, e$message))
                        })
                }
        }
}

# Main processing loop -----------------------------------------------------

for (time in times) {
        for (outcome in outcomes) {
                for (model in models) {
                        
                        if (outcome %in% c("MACE", "HF")) {
                                pattern_file <- glue("status_1_{model}_fu_{time}")
                        } else if (outcome %in% c("Non_CV_Death")) {
                                pattern_file <- glue("status_2_{model}_fu_{time}")
                        }
                        print(paste("Processing:", time, outcome, model, "->", pattern_file))
                        
                        setwd("/Users/Andrew/Documents/Work Documents/Postgraduate/Research/Current Papers/UDMI IPDMA/Cohort preparation")
                        
                        print(glue("Looking for pattern: {outcome} {pattern_file}"))
                        print("Matching data frame names:")
                        print(grep(pattern_file, names(loaded_dfs), value = TRUE))
                        
                        # Pool all cs HR ---------------------------------------------------------
                        df_pool <- bind_matching_dfs(glue("{pattern_file}$"), names(loaded_dfs), temp_env)
                        
                        if (nrow(df_pool) == 0) {
                                message(glue("⚠️ No matching data for pattern:{outcome} {pattern_file} — skipping."))
                                next
                        }
                        
                        # Check required columns
                        required_df_pool_cols <- c("term", "file_name")
                        missing_cols <- setdiff(required_df_pool_cols, names(df_pool))
                        if (length(missing_cols) > 0) {
                                message(glue("❌ Required columns missing in df_pool: {paste(missing_cols, collapse=', ')} — skipping."))
                                next
                        }
                        
                        # Remove adjustment vars --------------------------------------------------
                        terms_to_extract <- c(
                                "udmi4Type 1 MI",
                                "udmi4Type 2 MI",
                                "udmi4Acute myocardial injury",
                                "udmi4Chronic myocardial injury"
                        )
                        
                        df_pool <- df_pool %>% filter(term %in% terms_to_extract)
                        
                        # Extract Cohort name ---------------------------------------------------
                        df_pool$cohort <- sub(".*_(.*?)\\-2025.*", "\\1", df_pool$file_name)
                        
                        # Identify columns for summary files -------------------------------------
                        n_col <- "n"
                        if (outcome == "MACE") {
                                event_col <- glue("MI.or.CVD.{time}_events")
                        } else if (outcome == "Non_CV_Death") {
                                event_col <- glue("NonCV.death.{time}_events")
                        } else if (outcome == "HF") {
                                event_col <- glue("HF.{time}_events")
                        } else if (outcome == "All_cause_Death") {
                                event_col <- glue("death.{time}_events")
                        }
                        print(event_col)
                        
                        # Filter only event summary CSV files ------------------------------------
                        event_summary_paths <- loaded_dfs[grepl("_event_summary.*\\.csv$", loaded_dfs)]
                        
                        if (length(event_summary_paths) == 0) {
                                message("No event summary CSV files found. Skipping iteration.")
                                next
                        }
                        
                        # Read and process event summary files -----------------------------------
                        summary_dfs <- lapply(event_summary_paths, function(file_) {
                                df <- read.csv(file_)
                                
                                required_cols <- c("udmi4", n_col, event_col)
                                
                                if (!all(required_cols %in% colnames(df))) {
                                        message(glue("Skipping file {basename(file_)}: missing columns {paste(setdiff(required_cols, colnames(df)), collapse=', ')}"))
                                        return(NULL)
                                }
                                
                                df <- df %>% select(all_of(required_cols))
                                
                                cohort_name <- basename(file_)  # get filename only once
                                
                                if (grepl("High-STEACS", cohort_name)) {
                                        cohort <- sub("High-STEACS.*", "High-STEACS", cohort_name)
                                } else {
                                        cohort <- sub("-.*", "", cohort_name)
                                }
                                
                                df$cohort <- cohort
                                
                                df$term <- paste0("udmi4", df$udmi4)
                                df$udmi4 <- NULL
                                return(df)
                        })
                        
                        summary_dfs <- Filter(Negate(is.null), summary_dfs)
                        
                        combine <- do.call(rbind, summary_dfs)
                        
                        # merge to our pool data frame
                        csHR_pool <- left_join(df_pool, combine, by = c("term", "cohort"))
                        
                        if (nrow(combine) == 0) {
                                message("⚠️ No summary data found to merge — skipping iteration.")
                                next
                        }
                        
                        # Save output --------------------------------------------------------------
                        setwd("~/Documents/Work Documents/Postgraduate/Research/Current Papers/UDMI IPDMA/Meta-analysis/Meta-analysis")
                        
                        dir.create("Tables", showWarnings = FALSE)
                        dir.create("Tables/cs", showWarnings = FALSE)
                        dir.create(glue("Tables/cs/{time}"), showWarnings = FALSE)
                        
                        write.csv(csHR_pool, glue("Tables/cs/{time}/combined_csHR_{outcome}_{time}_{model}.csv"), row.names = FALSE)
                        print("Combined csHR estimates saved as .csv file")
                }
        }
}

setwd("~/Documents/Work Documents/Postgraduate/Research/Current Papers/UDMI IPDMA/Meta-analysis/Meta-analysis")

