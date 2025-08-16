# Load required libraries
library(ggplot2)
library(dplyr)
library(stringr)

# Step 1: Clean event labels and extract timepoint
plot_data <- heatmap_data %>%
        filter(udmi4 != "SUMMARY", cohort != "ALL") %>%
        mutate(
                Timepoint = case_when(
                        str_detect(event, "1y") ~ "1y",
                        str_detect(event, "5y") ~ "5y",
                        TRUE ~ NA_character_
                ),
                Category = str_remove(event, "\\.(1y|5y)_events") %>%
                        str_replace_all("_", " ") %>%
                        str_to_title()
        ) %>%
        rename(
                Cohort = cohort,
                EventRate = event_rate
        ) 

plot_data$Cohort <- factor(
        plot_data$Cohort,
        levels = c("ACTION","APACE","BACC","High-STEACS","KAROLINSKA","Edinburgh", "Olmsted", "Historic")
)

plot_data$udmi4 <- factor(
        plot_data$udmi4,
        levels = c("Type 1 MI", "Type 2 MI", "Acute myocardial injury", "Chronic myocardial injury", "No myocardial injury"),
        labels = c("Type 1 MI", "Type 2 MI", "Acute injury", "Chronic injury", "No injury")
)

plot_data$Timepoint <- factor(
        plot_data$Timepoint,
        levels=c("1y","5y"),
        labels=c("1 year", "5 years")
)

plot_data$Category <- factor(
        plot_data$Category,
        levels = c("Noncv.death","Mi.or.cvd","Cv.death","Ami", "Death"),
        labels = c("Non-CV death","MI or CV death","CV death","MI", "Death")
)

# Step 2: Aggregate to avoid overplotting
plot_data_clean <- plot_data %>%
        group_by(Cohort, udmi4, Category, Timepoint) %>%
        summarise(EventRate = mean(EventRate, na.rm = TRUE), .groups = "drop") %>%
        group_by(Cohort, Timepoint) %>%
        filter(!(Timepoint == "5y" & all(EventRate == 0))) %>%
        ungroup()

plot_data_clean <- plot_data_clean %>%
        mutate(
                FillGroup = ifelse(EventRate == 0, "zero", "nonzero")
        )

# Step 3: Plot heatmap with orange–red color scale
ggplot(plot_data_clean, aes(x = Cohort, y = Category, fill = EventRate)) +
        geom_tile(color = "white", linewidth = 0.5) +
        geom_text(
                aes(
                        label = sprintf("%.1f%%", EventRate),
                        color = EventRate == 0
                ),
                size = 3
        ) +
        scale_color_manual(
                values = c("TRUE" = "gray20", "FALSE" = "black"),
                guide = "none"
        )+
        scale_fill_gradientn(
                colours = c("#fdd0a2", "#fdae6b", "#e6550d", "#a63603"),
                values = scales::rescale(c(0, 5, 20, max(plot_data_clean$EventRate, na.rm = TRUE))),
                limits = c(0, max(plot_data_clean$EventRate, na.rm = TRUE)),
                name = "Event Rate (%)"
        )+
        facet_grid(udmi4 ~ Timepoint, scales = "free_y", space = "free") +
        theme_minimal(base_size = 14) +
        theme(
                axis.text.x = element_text(angle = 45, hjust = 1),
                panel.grid = element_blank(),
                strip.background = element_rect(fill = "#fce6d3", color = NA),
                strip.text = element_text(face = "bold")
        ) +
        labs(
                title = "",
                subtitle = "",
                x = "Cohort", y = "Event Category \n"
        )



