## Cartogram

# Load required libraries
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(dplyr)

# Create output directory
dir.create("Figures/Cartogram", recursive = TRUE, showWarnings = FALSE)

# Define countries to highlight
highlight_countries <- c(
        "United Kingdom",
        "Denmark",
        "Sweden",
        "United States of America",
        "Germany",
        "Switzerland",
        "Spain",
        "Italy",
        "Poland",
        "Czechia"  # 'Czech Republic' is named 'Czechia' in most shapefiles
)

# Load world map (as sf object)
world <- ne_countries(scale = "medium", returnclass = "sf")

# Add highlight column
world <- world %>%
        mutate(highlight = ifelse(admin %in% highlight_countries, "Highlighted", "Other"))

# Plot
p <- ggplot(world) +
        geom_sf(aes(fill = highlight), color = "white", size = 0.2) +
        scale_fill_manual(values = c("Highlighted" = "#B30000", "Other" = "gray90")) +
        theme_minimal() +
        labs(title = "UDMI IPDMA Countries",
             caption = "Countries: UK, Denmark, Sweden, USA, Germany, Switzerland, Spain, Italy, Poland, Czech Republic") +
        theme(
                legend.position = "none",
                plot.title = element_text(size = 16, face = "bold"),
                plot.caption = element_text(size = 10, hjust = 0.5),
                panel.background = element_rect(fill = "white"),
                panel.grid = element_blank()
        )

# Save plot
ggsave("Figures/Cartogram/cartogram_highlighted_countries.png", plot = p, width = 10, height = 6, dpi = 300)
