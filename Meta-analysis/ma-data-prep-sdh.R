# Setup --------------------------------------------------------------------

setwd("/Users/Andrew/Documents/Work Documents/Postgraduate/Research/Current Papers/UDMI IPDMA/Cohort preparation")

# Define wrapper vectors ---------------------------------------------------

times <- c("1y", "5y")
outcomes <- c("mace", "ncv", "hf")
models <- c("adj_primary_model", "adj_secondary_model")

# Create temp environment and load CSVs ------------------------------------

temp_env <- new.env()
loaded_dfs <- c()
study_folders <- list.dirs(full.names = TRUE, recursive = FALSE)

for (study_path in study_folders) {
        study_name <- basename(study_path)
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

# Define helper function ---------------------------------------------------

bind_matching_dfs <- function(pattern, df_names, env) {
        matches <- grep(pattern, df_names, value = TRUE)
        matches <- matches[!grepl("_sa_|Edinburgh|Action|Olmsted", matches)]
        matches <- matches[grepl(glue("{outcome}"), matches)]
        dfs <- lapply(matches, function(nm) get(nm, envir = env))
        bind_rows(dfs)
}

# Main wrapper loop --------------------------------------------------------

for (time in times) {
        for (outcome in outcomes) {
                for (model in models) {
                        
                        setwd("/Users/Andrew/Documents/Work Documents/Postgraduate/Research/Current Papers/UDMI IPDMA/Cohort preparation")
                        
                        if (outcome == "hf" || outcome == "acd") {
                                if (model != "adj_primary_model") next  # Only primary model used
                                pattern_file <- glue("fg_{outcome}_adj_primary_model_fu_{time}")
                        } else {
                                pattern_file <- glue("fg_{outcome}_{model}_fu_{time}")
                        }
                        
                        print(glue("Processing: {time} {outcome} {model} -> {pattern_file}"))
                        
                        # Pool all sHR --------------------------------------------------------
                        df_pool <- bind_matching_dfs(glue("{pattern_file}.*$"), names(loaded_dfs), temp_env)
                        
                        if (nrow(df_pool) == 0) {
                                message(glue("⚠️ No matching data for pattern:{outcome} {pattern_file} — skipping."))
                                next
                        }
                        
                        # Filter terms
                        terms_to_extract <- c(
                                "udmi4Type 1 MI",
                                "udmi4Type 2 MI",
                                "udmi4Acute myocardial injury",
                                "udmi4Chronic myocardial injury"
                        )
                        df_pool <- df_pool %>% filter(term %in% terms_to_extract)
                        
                        # Add cohort
                        df_pool$cohort <- sub(".*_(.*?)\\-2025.*", "\\1", df_pool$file_name)
                        
                        # Define event column
                        n_col <- "n"
                        if (outcome == "mace") {
                                event_col <- glue("MI.or.CVD.{time}_events")
                        } else if (outcome == "ncv") {
                                event_col <- glue("NonCV.death.{time}_events")
                        } else if (outcome == "hf") {
                                event_col <- glue("HF.{time}_events")
                        } 
                        print(glue("Using event column: {event_col}"))
                        
                        # Filter event summary files
                        event_summary_paths <- loaded_dfs[grepl("_event_summary.*\\.csv$", loaded_dfs)]
                        if (length(event_summary_paths) == 0) {
                                message("No event summary CSV files found. Skipping iteration.")
                                next
                        }
                        
                        # Read and process summary files
                        summary_dfs <- lapply(event_summary_paths, function(file_) {
                                df <- tryCatch({
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
                                }, error = function(e) {
                                        message(glue("❌ Error reading summary file {basename(file_)}: {e$message}"))
                                        return(NULL)
                                })
                                return(df)
                        })
                        
                        summary_dfs <- Filter(Negate(is.null), summary_dfs)
                        combine <- bind_rows(summary_dfs)
                        if (nrow(combine) == 0) {
                                message("⚠️ No summary data found to merge — skipping iteration.")
                                next
                        }
                        
                        # Merge pooled and summary
                        sHR_pool <- left_join(df_pool, combine, by = c("term", "cohort"))
                        
                        # Save output
                        setwd("~/Documents/Work Documents/Postgraduate/Research/Current Papers/UDMI IPDMA/Meta-analysis/Meta-analysis")
                        
                        dir.create("Tables", showWarnings = FALSE)
                        dir.create("Tables/sh", showWarnings = FALSE)
                        dir.create(glue("Tables/sh/{time}"), showWarnings = FALSE)
                        
                        write.csv(sHR_pool, glue("Tables/sh/{time}/combined_sHR_{outcome}_{time}_{model}.csv"), row.names = FALSE)
                        print(glue("✅ Combined sHR saved for {outcome}, {time}, {model}"))
                }
        }
}

setwd("~/Documents/Work Documents/Postgraduate/Research/Current Papers/UDMI IPDMA/Meta-analysis/Meta-analysis")

