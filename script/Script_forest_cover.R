# ==============================================================================
# Project: Land Cover Change in Protected Areas
# Author: Alexandra Lituma
# ==============================================================================

# -------------------------
# Load libraries
# -------------------------

library(tidyverse)

# -------------------------
# Import data
# -------------------------

datos <- readr::read_delim("data/Copia de STATISTICS_ECUADOR_COL2_V1.csv",
                           delim = ";",
                           skip = 1,
                           show_col_types = FALSE)

# -------------------------
# Data cleaning
# -------------------------

datos_long<- datos%>%
        pivot_longer(
                cols= starts_with("y"),
                names_to="year",
                values_to= "value"
        ) %>%
        mutate( 
                year=as.numeric(sub("y","",year)))

# -------------------------
# Analysis
# -------------------------

data_summary <- datos_long %>%
        filter(class_level_0 == "Natural", !is.na(value)) %>%
        group_by(year, class_level_1) %>%
        summarise(
                total_area = sum(value, na.rm = TRUE), .groups = "drop") %>%
        mutate(year = as.numeric(as.character(year)))

# -------------------------
# Change calculation (2018–2023)
# -------------------------

change_2018_2023 <- data_summary %>%
        filter(year %in% c(2018, 2023)) %>%
        select(year, class_level_1, total_area) %>%
        pivot_wider(names_from = year, values_from = total_area) %>%
        mutate(change_ha = `2023` - `2018`) %>%
        arrange(change_ha)

change_2018_2023

# -------------------------
# Visualization
# -------------------------

ggplot(data_summary,aes(x = year, y = total_area, color = class_level_1)) +
        geom_line(alpha = 0.5) +
        geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
        facet_wrap(~class_level_1, scales = "free_y") + 
                scale_x_continuous(
                breaks = 2018:2023, 
                limits = c(2018, 2023), 
                expand = expansion(mult = c(0.1, 0.1)) 
                ) +
        
        labs(
                title = "Land Cover Trends in Protected Areas of Ecuador (2018–2023)", 
                x = "Year", 
                y = "Hectares"
                ) +
        theme_minimal() +
        theme(
                axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        )


# -------------------------
# Output
# -------------------------

ggsave("outputs/forest_trend.png", width = 8, height = 5)



