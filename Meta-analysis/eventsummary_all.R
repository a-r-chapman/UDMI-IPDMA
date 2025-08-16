# Summary + breakdown of _event_summary.csv files
# Date: 18 July 2025

library(dplyr)
library(glue)
library(ggplot2)
library(tidyr)
library(stringr)

# Set working directory
setwd("/Users/Andrew/Documents/Work Documents/Postgraduate/Research/Current Papers/UDMI IPDMA/Cohort preparation")

# Find all relevant summary files
event_summary_paths <- list.files(
        path = ".",
        pattern = "_event_summary.*\\.csv$",
        recursive = TRUE,
        full.names = TRUE
)

if (length(event_summary_paths) == 0) {
        stop("❌ No _event_summary.csv files found.")
}

# Read and clean data
summary_dfs <- lapply(event_summary_paths, function(file_) {
        df <- tryCatch(read.csv(file_, fileEncoding = "UTF-8"), error = function(e) NULL)
        if (is.null(df) || !"n" %in% names(df) || !"udmi4" %in% names(df)) return(NULL)
        df$source_file <- basename(file_)
        return(df)
})

# Combine valid data frames
summary_dfs <- Filter(Negate(is.null), summary_dfs)
all_summary <- bind_rows(summary_dfs)

# Remove rows with NA udmi4
all_summary <- all_summary %>% filter(!is.na(udmi4))

# Extract cohort name from source_file
all_summary <- all_summary %>%
        mutate(
                cohort = sub("-.*", "", source_file),
                cohort = ifelse(cohort == "High", "High-STEACS", cohort)  # fix High name
        )

# List of relevant event columns
event_cols <- c(
        "AMI.1y_events", "AMI.5y_events",
        "death.1y_events", "death.5y_events",
        "MI.or.CVD.1y_events", "MI.or.CVD.5y_events",
        "NonCV.death.1y_events", "NonCV.death.5y_events", "CV.death.1y_events", "CV.death.5y_events"
)

# Check for missing event columns
missing_event_cols <- setdiff(event_cols, colnames(all_summary))
if (length(missing_event_cols) > 0) {
        warning(glue("⚠️ Missing event columns: {paste(missing_event_cols, collapse=', ')}"))
        event_cols <- intersect(event_cols, colnames(all_summary))  # Use available ones
}

# -----------------------------------
# Filtered summary (exclude 2 cohorts)
# -----------------------------------

# to filter by cohort or udmi4
summary_main <- all_summary #%>%
       #filter(!cohort %in% c("Historic"))

#summary_main <- all_summary #%>%
       #filter(!udmi4 %in% c("No myocardial injury"))

run_summary <- function(summary_main, event_cols, time_period = c("1y", "5y"), exclude_cohorts_5y = NULL) {
        time_period <- match.arg(time_period)
        
        # Filter summary_main if excluding cohorts in 5y period
        if(time_period == "5y" && !is.null(exclude_cohorts_5y)) {
                summary_main <- summary_main %>% filter(!cohort %in% exclude_cohorts_5y)
        }
        
        # Select event columns for this time period based on your column names pattern
        event_cols_time <- event_cols[str_detect(event_cols, paste0("\\.", time_period, "_events$"))]
        if(length(event_cols_time) == 0) {
                stop(paste0("No event columns found for time period '", time_period, "'. Check your event_cols vector."))
        }
        
        # Step 1: Base summary table by cohort
        summary_table <- summary_main %>%
                group_by(udmi4) %>%
                summarise(
                        total_n = sum(n, na.rm = TRUE),
                        across(all_of(event_cols_time), ~sum(.x, na.rm = TRUE)),
                        .groups = "drop"
                ) %>%
                as.data.frame()
        
        # Step 2: Add "All Patients" row with same filtering applied
        all_patients <- summary_main %>%
                summarise(
                        udmi4 = "All Patients",
                        total_n = sum(n, na.rm = TRUE),
                        across(all_of(event_cols_time), ~sum(.x, na.rm = TRUE))
                )
        
        # Step 3: Append all patients
        summary_table <- bind_rows(all_patients, summary_table)
        
        # Step 4: Reshape long for event counts
        summary_long <- summary_table %>%
                select(-total_n) %>%
                pivot_longer(
                        cols = -udmi4,
                        names_to = "event_time",
                        values_to = "value"
                ) %>%
                mutate(
                        time = case_when(
                                str_detect(event_time, paste0("\\.", time_period, "_events$")) ~ time_period,
                                TRUE ~ "Total"
                        ),
                        event = event_time %>%
                                str_replace_all(paste0("\\.", time_period, "_events$"), "") %>%
                                str_replace_all("\\.", " ") %>%
                                str_to_sentence()
                ) %>%
                select(udmi4, time, event, value)
        
        # Step 5: Add total_n for proportions
        totals <- summary_table %>% select(udmi4, total_n)
        
        summary_long <- summary_long %>%
                left_join(totals, by = "udmi4") %>%
                mutate(
                        percent = round((value / total_n) * 100, 1),
                        label = paste0(value, " (", percent, "%)")
                )
        
        # Step 6: Pivot wider for final table
        summary_wide <- summary_long %>%
                select(udmi4, time, event, label) %>%
                pivot_wider(
                        names_from = udmi4,
                        values_from = label
                )
        
        # Step 7: Add total_n row (as characters)
        total_row <- totals %>%
                pivot_wider(names_from = udmi4, values_from = total_n) %>%
                mutate(time = "Total N", event = "") %>%
                select(time, event, everything()) %>%
                mutate(across(-c(time, event), as.character))
        
        # Step 8: Combine total_n row and event summary
        final_table <- bind_rows(total_row, summary_wide) %>%
                rename(Time = time, Outcome = event)
        
        # Step 9: Reorder columns (optional)
        desired_order <- c(
                "All Patients", 
                "No myocardial injury", 
                "Type 1 MI", 
                "Type 2 MI", 
                "Acute myocardial injury", 
                "Chronic myocardial injury"
        )
        
        existing_cols <- intersect(desired_order, colnames(final_table))
        
        final_table <- final_table %>%
                select(Time, Outcome, all_of(existing_cols))
        
        # Custom order for Outcomes including empty string for Total N row
        desired_event_order <- c(
                "",                   # for Total N row
                "Mi or cvd",
                "Noncv death",
                "Ami",
                "Cv death",
                "Death"
        )
        
        # Custom order for Time with Total N first
        desired_time_order <- c("Total N", "Total", "1y", "5y")
        
        final_table <- final_table %>%
                mutate(
                        Time = factor(Time, levels = desired_time_order),
                        Outcome = factor(Outcome, levels = desired_event_order)
                ) %>%
                arrange(Time, Outcome) %>%
                mutate(
                        Time = as.character(Time),
                        Outcome = as.character(Outcome)
                )
        
        return(final_table)
}


# ---- USAGE ----

# For 1-year events (no exclusions):
summary_1y <- run_summary(summary_main, event_cols, time_period = "1y")

# For 5-year events, excluding cohorts 
summary_5y <- run_summary(summary_main, event_cols, time_period = "5y", exclude_cohorts_5y = c("ACTION","Historic"))

# Print results
summary_final <- rbind(summary_1y,summary_5y)

write.csv(summary_final, "udmi_event_summary_table.csv", row.names = FALSE)


df <- summary_table

df$patient_years <- df$total_n * 5

# Calculate events per patient-year for each 5y event
df$AMI_per_py <- df$AMI.5y_events / df$patient_years * 1000
df$death_per_py <- df$death.5y_events / df$patient_years * 1000
df$MIorCVD_per_py <- df$MI.or.CVD.5y_events / df$patient_years * 1000
df$NonCV.death_per_py <- df$NonCV.death.5y_events / df$patient_years * 1000

# View result
df[, c("udmi4", "AMI_per_py", "death_per_py", "MIorCVD_per_py", "NonCV.death_per_py")]


# Add TOTAL row
summary_total_main <- summary_main %>%
        summarise(
                cohort = "TOTAL",
                total_n = sum(n, na.rm = TRUE),
                across(all_of(event_cols), ~sum(.x, na.rm = TRUE))
        )

summary_table <- bind_rows(summary_table, summary_total_main)

data.frame(summary_table)

summary_udmi <- summary_main %>%
        group_by(cohort, udmi4) %>%
        summarise(
                total_n = sum(n, na.rm = TRUE),
                across(all_of(event_cols), ~sum(.x, na.rm = TRUE)),
                .groups = "drop"
        ) %>%
        bind_rows(
                summary_main %>%
                        summarise(
                                total_n = sum(n, na.rm = TRUE),
                                across(all_of(event_cols), ~sum(.x, na.rm = TRUE))
                        ) %>%
                        mutate(udmi4 = "SUMMARY")
        )

t1 <- data.frame(summary_udmi)
write.csv(t1, file="event_rate.csv")

summary_main %>%
        filter(cohort %in% c("APACE", "Edinburgh", "KAROLINSKA", "High-STEACS", "Historic", "ACTION")) %>%
        group_by(udmi4) %>%
        summarise(total_n = sum(n, na.rm = TRUE), .groups = "drop")

summary_cohortudmi <- summary_main %>%
        group_by(cohort, udmi4) %>%
        summarise(total_n = sum(n, na.rm = TRUE), .groups = "drop") %>%
        group_by(cohort) %>%
        mutate(proportion = total_n / sum(total_n)) %>%
        ungroup()
print(summary_cohortudmi)

# Save to CSV
write.csv(summary_table, "udmi4_event_summary_table.csv", row.names = FALSE)
cat("✅ Saved filtered summary: udmi4_event_summary_table.csv\n")

# -----------------------------------
# Full summary (all cohorts included)
# -----------------------------------

summary_table_all <- all_summary %>%
        group_by(cohort) %>%
        summarise(
                total_n = sum(n, na.rm = TRUE),
                across(all_of(event_cols), ~sum(.x, na.rm = TRUE)),
                .groups = "drop"
        )

# Add TOTAL row
summary_total_all <- all_summary %>%
        summarise(
                cohort = "TOTAL",
                total_n = sum(n, na.rm = TRUE),
                across(all_of(event_cols), ~sum(.x, na.rm = TRUE))
        )

summary_table_all <- bind_rows(summary_table_all, summary_total_all)

# Save full summary
write.csv(summary_table_all, "udmi4_event_summary_table_ALL.csv", row.names = FALSE)
cat("✅ Saved full summary: udmi4_event_summary_table_ALL.csv\n")


### Heat map 
library(dplyr)
library(tidyr)

# Assume: `event_cols` is a vector of event column names like c("event1", "event2", ...)

# Summarize by cohort and udmi4
summary_udmi <- all_summary %>%
        group_by(cohort, udmi4) %>%
        summarise(
                total_n = sum(n, na.rm = TRUE),
                across(all_of(event_cols), ~ sum(.x, na.rm = TRUE)),
                .groups = "drop"
        ) %>%
        mutate(proportion = total_n / sum(total_n))

# Add total SUMMARY row
summary_totals <- summary_main %>%
        summarise(
                total_n = sum(n, na.rm = TRUE),
                across(all_of(event_cols), ~ sum(.x, na.rm = TRUE))
        ) %>%
        mutate(cohort = "ALL", udmi4 = "SUMMARY")

# Combine
summary_combined <- bind_rows(summary_udmi, summary_totals)

# Reshape to long format: one row per event type
heatmap_data <- summary_combined %>%
        pivot_longer(
                cols = all_of(event_cols),
                names_to = "event",
                values_to = "event_count"
        ) %>%
        mutate(event_rate = 100 * event_count / total_n) %>%
        select(cohort, udmi4, event, event_rate)

