# Data preparation for cause specific HR endpoint - sensitivity analysis
# Andrew Chapman and Pedro Lopez Ayala
# 15 June 2025
# R version 4.2.3

setwd("/Users/Andrew/Documents/Work Documents/Postgraduate/Research/Current Papers/UDMI IPDMA/Cohort preparation")
 
rm(list=c("times","outcomes","models"))

# Define vectors to loop over
times <- c("1y", "5y")
outcomes <- c("MACE", "Non_CV_Death")
models <- c("adjusted_pri_model", "adjusted_sec_model", "unadjusted")

# Create temp environment and load data
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

# Define helper to bind matching data frames
bind_matching_dfs <- function(pattern, df_names, env, outcome) {
        matches <- grep(pattern, df_names, value = TRUE)
        matches <- matches[grepl("_sa_|Olmsted", matches)]
        matches <- matches[grepl(glue("{outcome}"), matches)]
        dfs <- lapply(matches, function(nm) get(nm, envir = env))
        bind_rows(dfs)
}


# Begin looping over time, outcome, and model combinations
for (time in times) {
        for (outcome in outcomes) {
                for (model in models) {
                        
                        outcomes <- c("MACE", "Non_CV_Death")
                        stopifnot(identical(outcomes, c("MACE", "Non_CV_Death")))
                        message("✅ Outcomes correctly set: ", paste(outcomes, collapse = ", "))
                        
                        if (outcome %in% c("MACE")) {
                                pattern_file <- glue("status_1_{model}_fu_{time}")
                        } else if (outcome %in% c("Non_CV_Death")) {
                                pattern_file <- glue("status_2_{model}_fu_{time}")
                        }
                        
                        message(glue("⏳ Processing: {time} | {outcome} | {model}"))
                        
                        # Pool all cs HR
                        df_sa_pool <- bind_matching_dfs(glue("{pattern_file}$"), names(loaded_dfs), temp_env, outcome)
                        
                        if (nrow(df_sa_pool) == 0) {
                                message("⚠️ No matching data found for this combination — skipping.")
                                next
                        }
                        
                        # Filter only UDMI terms of interest
                        terms_to_extract <- c(
                                "udmi4Type 1 MI",
                                "udmi4Type 2 MI",
                                "udmi4Acute myocardial injury",
                                "udmi4Chronic myocardial injury"
                        )
                        df_sa_pool <- df_sa_pool %>% filter(term %in% terms_to_extract)
                        df_sa_pool$cohort <- sub(".*_(.*?)\\-2025.*", "\\1", df_sa_pool$file_name)
                        
                        # Define event column
                        n_col <- "n"
                        event_col <- switch(outcome,
                                            "MACE" = glue("MI.or.CVD.{time}_events"),
                                            "Non_CV_Death" = glue("NonCV.death.{time}_events"))
                        
                        # Filter and read summary files
                        event_summary_paths <- loaded_dfs[grepl("_event_summary.*\\.csv$", loaded_dfs)]
                        if (length(event_summary_paths) == 0) {
                                message("⚠️ No summary files found — skipping.")
                                next
                        }
                        
                        summary_dfs <- lapply(event_summary_paths, function(file_) {
                                df <- tryCatch({
                                        df <- read.csv(file_) %>% select(all_of(c("udmi4", n_col, event_col)))
                                        
                                        cohort_name <- basename(file_)  # get filename only once
                                        
                                        if (grepl("High-STEACS", cohort_name)) {
                                                cohort <- sub("High-STEACS.*", "High-STEACS", cohort_name)
                                        } else {
                                                cohort <- sub("-.*", "", cohort_name)
                                        }
                                        
                                        df$cohort <- cohort
                                        
                                        df$term <- paste0("udmi4", df$udmi4)
                                        df$udmi4 <- NULL
                                        df
                                }, error = function(e) {
                                        message(glue("⚠️ Skipping {basename(file_)} — missing expected columns"))
                                        NULL
                                })
                                return(df)
                        })
                        
                        summary_dfs <- Filter(Negate(is.null), summary_dfs)
                        if (length(summary_dfs) == 0) {
                                message("⚠️ No usable summary data — skipping.")
                                next
                        }
                        
                        combine <- do.call(rbind, summary_dfs)
                        csHR_sa_pool <- left_join(df_sa_pool, combine, by = c("term", "cohort"))
                        
                        # Save output
                        output_dir <- glue("~/Documents/Work Documents/Postgraduate/Research/Current Papers/UDMI IPDMA/Meta-analysis/Meta-analysis/Tables/cs/{time}")
                        dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
                        
                        out_file <- glue("{output_dir}/combined_csHR_sa_{outcome}_{time}_{model}.csv")
                        write.csv(csHR_sa_pool, out_file, row.names = FALSE)
                        message(glue("✅ Saved: {basename(out_file)}"))
                }
        }
}

# Reset working directory
setwd("~/Documents/Work Documents/Postgraduate/Research/Current Papers/UDMI IPDMA/Meta-analysis/Meta-analysis")
