library(dplyr)
library(ggplot2)
library(glue)
library(scales)
library(ggtext)

# Author-year lookup table [use this if journal wants]
# author_year_lookup <- c(
#         "BACC" = "Sörensen et al 2019",
#         "High-STEACS" = "Shah et al 2018",
#         "Historic" = "Anand et al 2021",
#         "KAROLINSKA" = "Kadesjö et al 2019",
#         "APACE" = "Nestelberger et al 2022",
#         "ACTION" = "Knott et al 2022",
#         "Edinburgh" = "Chapman et al 2018",
#         "Olmsted" = "Raphael et al 2020"
# )

author_year_lookup <- c(
        "BACC" = "BACC",
        "High-STEACS" = "High-STEACS",
        "Historic" = "HISTORIC",
        "KAROLINSKA" = "KAROLINSKA",
        "APACE" = "APACE",
        "ACTION" = "ACTION",
        "Edinburgh" = "EDINBURGH",
        "Olmsted" = "OLMSTED"
)

# Label map (used for plotting)
term_labels <- c(
        "udmi4Type 1 MI" = "Type 1 MI",
        "udmi4Type 2 MI" = "Type 2 MI",
        "udmi4Acute myocardial injury" = "Acute myocardial injury",
        "udmi4Chronic myocardial injury" = "Chronic myocardial injury"
)

pattern_terms <- unname(term_labels)  # Used for facet order

# Create all necessary folders
dir.create("Figures", showWarnings = FALSE)
for (ma in c("cs", "sh")) {
        for (time in c("1y", "5y")) {
                dir.create(glue("Figures/{ma}/{time}"), recursive = TRUE, showWarnings = FALSE)
        }
}

# Loop through meta-analysis types
mas <- c("cs", "sh")

for (ma in mas) {
        
        # Set parameters depending on ma type
        if (ma == "cs") {
                times <- c("1y", "5y")
                outcomes <- c("MACE", "Non_CV_Death", "HF")
                models <- c("adjusted_pri_model", "adjusted_sec_model", "unadjusted")
        } else if (ma == "sh") {
                times <- c("1y", "5y")
                outcomes <- c("mace", "ncv", "hf")
                models <- c("adj_primary_model", "adj_secondary_model")
        }
        
        # Loop through combinations
        for (time in times) {
                for (outcome in outcomes) {
                        for (model in models) {
                                
                                ma_file <- ifelse(ma == "sh", "s", ma)
                                obj_name <- glue("bias_combined_{ma_file}hr_{time}_{outcome}_{model}")
                                
                                if (!exists(obj_name)) {
                                        message(glue("⏭️ Skipping: {obj_name} not found"))
                                        next
                                }
                                
                                df_model <- get(obj_name)
                                
                                df_model <- df_model %>%
                                        filter(outcome %in% pattern_terms) %>%
                                        mutate(
                                                outcome = factor(outcome, levels = pattern_terms),
                                                ci_lower = conf.low,
                                                ci_upper = conf.high,
                                                HR = HR,
                                                HR_lower = ci_lower,
                                                HR_upper = ci_upper,
                                                weight_scaled = ifelse(!is.na(weight), scales::rescale(weight, to = c(1.5, 4)), NA),
                                                psize = ifelse(type == "Summary", 4.5, weight_scaled)
                                        ) %>%
                                        group_by(outcome) %>%
                                        mutate(
                                                study_display = ifelse(
                                                        type == "Summary",
                                                        "Meta-Estimate",
                                                        ifelse(study %in% names(author_year_lookup),
                                                               author_year_lookup[study],
                                                               study)
                                                )
                                        ) %>%
                                        arrange(outcome, type == "Summary", .by_group = TRUE) %>%
                                        mutate(y_pos = row_number()) %>%
                                        ungroup() %>%
                                        mutate(
                                                # Adjusted positions for n/Total and HR labels, further left to avoid overlap
                                                x_P = 0.001,
                                                x_HR = 0.008
                                        )
                                
                                df_model <- df_model %>% arrange(outcome, y_pos)
                                
                                # Extract heterogeneity text from Summary row
                                i2_labels <- df_model %>%
                                        filter(type == "Summary") %>%
                                        select(outcome, text = heterogeneity) %>%
                                        left_join(
                                                df_model %>%
                                                        group_by(outcome) %>%
                                                        summarise(n_studies = n_distinct(study)),
                                                by = "outcome"
                                        ) %>%
                                        mutate(
                                                xpos = 0.001,
                                                ypos = -(n_studies + 1.5)  # Adjust spacing as needed
                                        )
                                
                                header_labels <- tibble(
                                        outcome   = factor(pattern_terms, levels = pattern_terms),
                                        x_P       = 0.001,
                                        x_HR      = 0.008,
                                        y         = -0.15,
                                        label_P   = "n/Total (%)",
                                        label_HR  = paste0(ma_file, "HR (95% CI)")
                                )
                                
                                # Forest plot
                                p <- ggplot(df_model, aes(x = HR, y = -y_pos)) +
                                        geom_point(aes(size = psize, shape = type,
                                                       fill = type, colour = type)) +
                                        geom_errorbarh(aes(xmin = HR_lower, xmax = HR_upper),
                                                       height = 0.15, linewidth = 0.6, colour = "black") +
                                        geom_text(aes(x = x_P, label = P_label, fontface = ifelse(type == "Summary", "bold", "plain")),
                                                  hjust = 0, size = 3.8, family = "Helvetica") +
                                        geom_text(aes(x = x_HR, label = HR_label, fontface = ifelse(type == "Summary", "bold", "plain")),
                                                  hjust = 0, size = 3.8, family = "Helvetica") +
                                        scale_y_continuous(
                                                breaks = -df_model$y_pos,
                                                labels = with(df_model,
                                                              ifelse(type == "Summary",
                                                                     paste0("<b>", study_display, "</b>"),
                                                                     paste0("<i>", study_display, "</i>"))),
                                                expand = expansion(mult = c(0.05, 0.15))
                                        ) +
                                        geom_vline(xintercept = 1, linetype = "dashed", colour = "grey30") +
                                        scale_x_log10(
                                                name = glue("{ma_file}HR (95% CI)"),
                                                breaks = c(0.25, 0.5, 1, 2, 4, 8),
                                                labels = c("0.25", "0.5", "1", "2", "4", "8"),
                                                expand = expansion(mult = c(0.05, 0.2))
                                        ) +
                                        scale_shape_manual(values = c("Study" = 15, "Summary" = 18)) +  # squares and diamonds original sizes
                                        scale_fill_manual(values = c("Study" = "black", "Summary" = "#B30000")) +
                                        scale_colour_manual(values = c("Study" = "black", "Summary" = "#B30000")) +
                                        guides(size = "none", shape = "none", fill = "none", colour = "none") +
                                        facet_wrap(~ outcome, ncol = 1, scales = "free_y", strip.position = "top") +
                                        geom_text(data = i2_labels,
                                                  aes(x = xpos, y = ypos, label = text, group = outcome),
                                                  inherit.aes = FALSE, hjust = 0, size = 3.5, family = "Helvetica") +
                                        geom_text(data = header_labels,
                                                  aes(x = x_P, y = y, label = label_P, group = outcome),
                                                  inherit.aes = FALSE, hjust = 0, size = 4.1,
                                                  fontface = "bold", family = "Helvetica") +
                                        geom_text(data = header_labels,
                                                  aes(x = x_HR, y = y, label = label_HR, group = outcome),
                                                  inherit.aes = FALSE, hjust = 0, size = 4.1,
                                                  fontface = "bold", family = "Helvetica") +
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
                                                strip.text = element_text(hjust = 0.25, size = 13, face = "bold"),
                                                strip.placement = "outside",
                                                strip.switch.pad.wrap = unit(1.5, "lines"),
                                                plot.margin = margin(10, 10, 10, 10)
                                        )
                                
                                # Save plot
                                ggsave(glue("Figures/{ma}/{time}/bias_forest_plot_{outcome}_{model}_{time}.png"), p, width = 12, height = 8.5)
                                print(glue("✅ Saved forest plot: {ma} | {outcome} | {model} | {time}"))
                        }
                }
        }
}
