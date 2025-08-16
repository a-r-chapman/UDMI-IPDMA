# Code for plotting CIF plots 
# Output produces CIF plots for: 
# -- status 1 (AMI or CV death) and status 2 (non CV death) unadjusted (using Aalen Johnson estimator)
# -- status 1 (AMI or CV death) and status 2 (non CV death) adjusted (using Fine and Grays model)


# CIF Plot ----------------------------------------------------------------

df_cif <- df[complete.cases(df[, c("udmi4", "age", "sex", "gfr")]), ]

if (fu_time == "1y") {
  df_cif$time <- df_cif[[time]]
  df_cif$status <- df_cif[[status]]  # Numeric: 0 = censored, 1 = MACE, 2 = Non-CV death
  xmax <- 365
  break_time_x <- 90
  xlab <- "Follow up (days)"
} else if (fu_time == "5y") {
  df_cif$time <- df_cif[[time]] / 365
  df_cif$status <- df_cif[[status]] 
  xmax <- 5
  break_time_x <- 1
  xlab <- "Follow up (years)"
} 

## Unadjusted (aalen johansen estimator)----

fgr_model_1_unadj <- FGR(Hist(time, status) ~ udmi4, data = df_cif, cause = 1)
fgr_model_2_unadj <- FGR(Hist(time, status) ~ udmi4, data = df_cif, cause = 2)

cif_fgr_1 <- adjustedcif(data = df_cif,
                         variable = "udmi4", #grouping variable of interest, must be class factor
                         ev_time = "time", #time variable
                         event = "status", #event variable
                         method = "aalen_johansen", #uses the cuminc wrapper
                         cause = 1, #cause of interest, in this case 1
                         conf_int = TRUE)

cif_fgr_2 <- adjustedcif(data = df_cif,
                         variable = "udmi4", #grouping variable of interest, must be class factor
                         ev_time = "time", #time variable
                         event = "status", #event variable
                         method = "aalen_johansen", #uses the cuminc wrapper
                         cause = 2, #cause of interest, in this case 1
                         conf_int = TRUE)

### Plot----
custom_colors <- c(
  "Acute myocardial injury" = "#2166ac",            
  "Type 1 MI"               = "#b2182b",   
  "Type 2 MI"               = "#ef8a62",
  "Chronic myocardial injury" = "dark gray",               
  "No myocardial injury"    = "#1b7837"
)

p1 <- ggplot(cif_fgr_1$adj, aes(x = time, y = cif, color = group, fill = group)) +
  geom_line(linewidth = 0.5) + #size aesthetic for lines was deprecated in ggplot2 3.4.0
  scale_y_continuous(labels = label_percent()) +
  labs(
    x = xlab, 
    y = "Cumulative incidence",
    color = "Group",
    fill = "Group"
  ) +
  scale_color_manual(values = custom_colors) + # breaks = "Type 1 MI", "Type 2 MI", "Acute myocardial injury", "Chronic myocardial injury","No myocardial injury"
  scale_fill_manual(values = custom_colors) +
  coord_cartesian(ylim = c(0, 0.5)) +
  scale_x_continuous(breaks = seq(0, xmax, break_time_x)) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  ggtitle("MI or cardiovascular death") +
  theme(
    plot.title = "element_text"(
      face = "bold",
      size = 14,
      hjust = 0.5
    )
  )

p2 <- ggplot(cif_fgr_2$adj, aes(x = time, y = cif, color = group, fill = group)) +
  geom_line(linewidth = 0.5) +
  scale_y_continuous(labels = label_percent()) +
  labs(
    x = xlab, 
    y = "Cumulative incidence",
    color = "Group",
    fill = "Group") +
  scale_color_manual(values = custom_colors) + #, breaks = c("Type 1 MI", "Type 2 MI", "Acute myocardial injury", "Chronic myocardial injury","No myocardial injury")
  scale_fill_manual(values = custom_colors) +
  coord_cartesian(ylim = c(0, 0.5)) +
  scale_x_continuous(breaks = seq(0, xmax, break_time_x)) +
  theme_bw(base_size = 14) +
  theme(legend.position = "right",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + 
  ggtitle("Non cardiovascular death") +
  theme(
    plot.title = "element_text"(
      face = "bold",
      size = 14,
      hjust = 0.5
    )
  )

p3 <- p1 + p2 + plot_layout(ncol = 2, widths = c(1, 1), axis_titles = "collect") &
  theme(plot.margin = margin(5, 5, 5, 1)) 

p3

height <- 8
width <- 10
ggsave(glue("Figures/{fu_time}/CIF_unadj_fu_{fu_time}_{cohort}.svg"), width = width, height = height)
ggsave(glue("Figures/{fu_time}/CIF_unadj_fu_{fu_time}_{cohort}.png"), width = width, height = height)
ggsave(glue("Figures/{fu_time}/CIF_unadj_fu_{fu_time}_{cohort}.pdf"), width = width, height = height)
print("Aalen Johansen plot saved as .png file")

## Adjusted (Fine Gray model)----

fgr_model_1 <- FGR(Hist(time, status) ~ udmi4 + age + sex + gfr, data = df_cif, cause = 1)
fgr_model_2 <- FGR(Hist(time, status) ~ udmi4 + age + sex + gfr, data = df_cif, cause = 2)


### Plot----
# For plotting we will use the adjustedurves::adjustedcif() function
adjcif_fgr_1 <- adjustedcif(data = df_cif,
                            variable = "udmi4", #grouping variable of interest, must be class factor
                            ev_time = "time", #time variable
                            event = "status", #event variable
                            method = "direct", #direct standardization
                            outcome_model = fgr_model_1, #your model fitted previously
                            cause = 1, #cause of interest, in this case 1
                            conf_int = TRUE)


adjcif_fgr_2 <- adjustedcif(data = df_cif,
                            variable = "udmi4", #grouping variable of interest, must be class factor
                            ev_time = "time", #time variable
                            event = "status", #event variable
                            method = "direct", #direct standardization
                            outcome_model = fgr_model_2, #your model fitted previously
                            cause = 2, #cause of interest, in this case 1
                            conf_int = TRUE)


p1a <- ggplot(adjcif_fgr_1$adj, aes(x = time, y = cif, color = group, fill = group)) +
  geom_line(linewidth = 0.5) + #size aesthetic for lines was deprecated in ggplot2 3.4.0
  scale_y_continuous(labels = label_percent()) +
  labs(
    x = xlab, # may need to adjust the x axis for 1Y
    y = "Cumulative incidence (adjusted)",
    color = "Group",
    fill = "Group"
  ) +
  scale_color_manual(values = custom_colors) + # breaks = "Type 1 MI", "Type 2 MI", "Acute myocardial injury", "Chronic myocardial injury","No myocardial injury"
  scale_fill_manual(values = custom_colors) +
  coord_cartesian(ylim = c(0, 0.5)) +
  scale_x_continuous(breaks = seq(0, xmax, break_time_x)) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  ggtitle("MI or cardiovascular death") +
  theme(
    plot.title = "element_text"(
      face = "bold",
      size = 14,
      hjust = 0.5
    )
  )

p2a <- ggplot(adjcif_fgr_2$adj, aes(x = time, y = cif, color = group, fill = group)) +
  geom_line(linewidth = 0.5) +
  scale_y_continuous(labels = label_percent()) +
  labs(
    x = xlab, # may need to adjust the x axis for 1Y
    y = "Cumulative incidence (adjusted)",
    color = "Group",
    fill = "Group") +
  scale_color_manual(values = custom_colors) + #, breaks = c("Type 1 MI", "Type 2 MI", "Acute myocardial injury", "Chronic myocardial injury","No myocardial injury")
  scale_fill_manual(values = custom_colors) +
  coord_cartesian(ylim = c(0, 0.5)) +
  scale_x_continuous(breaks = seq(0, xmax, break_time_x)) +
  theme_bw(base_size = 14) +
  theme(legend.position = "right",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + 
  ggtitle("Non cardiovascular death") +
  theme(
    plot.title = "element_text"(
      face = "bold",
      size = 14,
      hjust = 0.5
    )
  )

p3a <- p1a + p2a + plot_layout(ncol = 2, widths = c(1, 1), axis_titles = "collect") &
  theme(plot.margin = margin(5, 5, 5, 1)) 

p3a

#### Save----
height <- 8
width <- 10
ggsave(glue("Figures/{fu_time}/CIF_adj_fu_{fu_time}_{cohort}.svg"), width = width, height = height)
ggsave(glue("Figures/{fu_time}/CIF_adj_fu_{fu_time}_{cohort}.png"), width = width, height = height)
ggsave(glue("Figures/{fu_time}/CIF_adj_fu_{fu_time}_{cohort}.pdf"), width = width, height = height)
print("CIF plot saved as .png file")
