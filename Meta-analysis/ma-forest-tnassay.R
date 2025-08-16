library(tidyverse)
library(ggplot2)
library(glue)
library(ggtext)

plot_forest <- function(file_path, ma_file) {
        df_model <- read_csv(file_path)
        
        # Standardize column names
        names(df_model)[6] <- "hs_ctn"
        names(df_model)[7] <- "outcome"
        
        # Lookup for formatting study names
        author_year_lookup <- c(
                "BACC" = "Sörensen et al 2019",
                "High-STEACS" = "Shah et al 2018",
                "Historic" = "Anand et al 2021",
                "KAROLINSKA" = "Kadesjö et al 2019",
                "APACE" = "Nestelberger et al 2022",
                "ACTION" = "Knott et al 2022",
                "Edinburgh" = "Chapman et al 2018",
                "Olmsted" = "Raphael et al 2020"
        )
        
        # Desired group label order
        desired_order <- c(
                "Type 1 MI - hs-cTnI", "Type 1 MI - hs-cTnT",
                "Type 2 MI - hs-cTnI", "Type 2 MI - hs-cTnT",
                "Acute myocardial injury - hs-cTnI",     "Acute myocardial injury - hs-cTnT",
                "Chronic myocardial injury - hs-cTnI",   "Chronic myocardial injury - hs-cTnT"
        )
        
        # Create unique group label and factor ordering
        df_model <- df_model %>%
                mutate(
                        hs_ctn = factor(hs_ctn, levels = c("hs-cTnI", "hs-cTnT")),
                        outcome = factor(outcome, levels = c("Type 1 MI", "Type 2 MI", "Acute myocardial injury", "Chronic myocardial injury"))
                ) %>%
                mutate(
                        group_label = factor(
                                paste(outcome, hs_ctn, sep = " - "),
                                levels = desired_order
                        )
                ) %>%
                arrange(group_label, type == "Summary") %>%
                group_by(group_label) %>%
                mutate(
                        y_pos = row_number(),
                        study_display = outcome,
                        x_P = 0.0015,
                        x_HR = 0.008,
                        ci_lower = conf.low,
                        ci_upper = conf.high,
                        HR_lower = ci_lower,
                        HR_upper = ci_upper,
                        weight_scaled = ifelse(!is.na(weight), scales::rescale(weight, to = c(1.5, 4)), NA),
                        psize = ifelse(type == "Summary", 4.5, weight_scaled)
                ) %>%
                ungroup() %>%
                arrange(group_label, y_pos) %>%
                mutate(global_y = row_number())
        
        # Heterogeneity position: below each biomarker block (Summary row only)
        # Ensure heterogeneity labels appear for all groups, even missing ones
        all_groups <- factor(desired_order, levels = desired_order)
        
        present_i2 <- df_model %>%
                filter(type == "Summary") %>%
                group_by(group_label) %>%
                summarise(
                        text = first(heterogeneity),
                        ypos = max(global_y) + 0.5,
                        xpos = 0.001,
                        .groups = "drop"
                )
        
        i2_labels <- tibble(group_label = all_groups) %>%
                left_join(present_i2, by = "group_label") %>%
                mutate(
                        text = ifelse(is.na(text), "N/A", text),
                        ypos = ifelse(is.na(ypos), max(df_model$global_y) + 1, ypos),
                        xpos = ifelse(is.na(xpos), 0.001, xpos)
                )
        
        # Add columns for plotting
        df_model <- df_model %>%
                mutate(
                        legend_group = ifelse(type == "Summary", as.character(hs_ctn), NA),
                        x_BIO = 0.0004  # new column position between outcome and n/Total
                )
        
        # Header labels (add one for hs-cTnX column)
        header_labels <- tibble(
                x_BIO = 0.0004,
                x_P = 0.0015,
                x_HR = 0.008,
                y = 0,
                label_BIO = "Assay",
                label_P = "n/Total (%)",
                label_HR = paste0(ma_file, " (95% CI)")
        )
        
        # Start plotting
        p <- ggplot(df_model, aes(x = HR, y = -global_y)) +
                geom_point(aes(
                        size = psize,
                        shape = legend_group,
                        fill = legend_group,
                        colour = legend_group
                )) +
                geom_errorbarh(aes(xmin = HR_lower, xmax = HR_upper),
                               height = 0.15, linewidth = 0.6, colour = "black") +
                # Outcome text already appears via y-axis labels
                
                # New biomarker text column (hs-cTnI / hs-cTnT)
                geom_text(aes(x = x_BIO, label = as.character(hs_ctn),
                              fontface = ifelse(type == "Summary", "bold", "plain")),
                          hjust = 0, size = 3.8, family = "Helvetica") +
                
                geom_text(aes(x = x_P, label = P_label,
                              fontface = ifelse(type == "Summary", "bold", "plain")),
                          hjust = 0, size = 3.8, family = "Helvetica") +
                
                geom_text(aes(x = x_HR, label = HR_label,
                              fontface = ifelse(type == "Summary", "bold", "plain")),
                          hjust = 0, size = 3.8, family = "Helvetica") +
                
                geom_vline(xintercept = 1, linetype = "dashed", colour = "grey30") +
                
                geom_text(data = i2_labels,
                          aes(x = 0.0004, y = -ypos, label = text),
                          inherit.aes = FALSE, hjust = 0, size = 3.5, family = "Helvetica") +
                
                geom_text(data = header_labels,
                          aes(x = x_BIO, y = y, label = label_BIO),
                          inherit.aes = FALSE, hjust = 0, size = 4.1, fontface = "bold", family = "Helvetica") +
                
                geom_text(data = header_labels,
                          aes(x = x_P, y = y, label = label_P),
                          inherit.aes = FALSE, hjust = 0, size = 4.1, fontface = "bold", family = "Helvetica") +
                
                geom_text(data = header_labels,
                          aes(x = x_HR, y = y, label = label_HR),
                          inherit.aes = FALSE, hjust = 0, size = 4.1, fontface = "bold", family = "Helvetica") +
                
                scale_y_continuous(
                        breaks = -df_model$global_y -0.04,
                        labels = with(df_model, ifelse(global_y %% 2 == 1,
                                                       ifelse(type == "Summary",
                                                              paste0("<b>", study_display, "</b>"),
                                                              paste0("<i>", study_display, "</i>")),
                                                       "")),
                        expand = expansion(mult = c(0.05, 0.15))
                )+
                scale_x_log10(
                        name = glue("{ma_file} (95% CI)"),
                        breaks = c(0.5, 1, 2, 4, 8),
                        labels = c("0.5", "1", "2", "4", "8"),
                        expand = expansion(mult = c(0.05, 0.2))
                ) +
                scale_shape_manual(
                        values = c("hs-cTnI" = 18, "hs-cTnT" = 18),
                        name = "hs-cTn Assay"
                ) +
                scale_fill_manual(
                        values = c("hs-cTnI" = "#B30000", "hs-cTnT" = "#0072B2"),
                        name = "hs-cTn Assay"
                ) +
                scale_colour_manual(
                        values = c("hs-cTnI" = "#B30000", "hs-cTnT" = "#0072B2"),
                        name = "hs-cTn Assay"
                ) +
                guides(
                        size = "none"
                ) +
                theme_minimal(base_family = "Helvetica") +
                theme(
                        panel.background = element_rect(fill = "white", colour = NA),
                        plot.background = element_rect(fill = "white", colour = NA),
                        panel.grid.major.y = element_blank(),
                        panel.grid.major.x = element_line(colour = "grey80", linewidth = 0.3),
                        panel.grid.minor = element_blank(),
                        axis.title.x = element_text(hjust = 0.5, size = 13, face = "bold"),
                        axis.title.y = element_blank(),
                        axis.text.y = element_markdown(size = 11, family = "Helvetica"),
                        axis.text.x = element_text(size = 11),  
                        axis.line.x = element_line(colour = "black"),
                        plot.margin = margin(10, 10, 10, 10),
                        legend.position = "right"
                )
        
        # Save plot
        ggsave(glue("tn_meta-estimate_{ma_file}.png"), plot = p, width = 10, height = 10, dpi = 300)
        
        return(p)
}

# Example calls:
plot_forest("Tables/cs/1y/meta_estimate_combined_sHR_1y_tn.csv", "sHR")
plot_forest("Tables/cs/1y/meta_estimate_combined_csHR_1y_tn.csv", "csHR")
