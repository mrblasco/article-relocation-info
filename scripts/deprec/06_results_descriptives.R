suppressMessages({
    library(dplyr)
    library(ggplot2)
    library(ggtext)
    library(patchwork)
    library(knitr)
    library(kableExtra)
})

source("R/theme.R")
theme_set(theme_minimal())

# ----------------------------------------------------
# Setup
# ----------------------------------------------------

results_dir <- file.path("results", c("figures", "tables", "models"))
sapply(results_dir, dir.create, recursive = TRUE, showWarnings = FALSE)

data_dir <- file.path("data", "processed")
ds_survey_filename <- file.path(data_dir, "fair_survey_clean.rds")
ds_long_filename <- file.path(data_dir, "fair_survey_long.rds")

# ----------------------------------------------------
# Load data
# ----------------------------------------------------

ds_long <- readRDS(ds_long_filename)
ds_survey <- readRDS(ds_survey_filename)

# ----------------------------------------------------
# 3.1.1 Perceived fairness
# ----------------------------------------------------

rank_by_country <- ds_long  %>% 
    bind_rows(., mutate(., country = "All")) %>%
    count(rank, relocation, country, relocation_treatment) %>%
    mutate(percent = n / sum(n), .by = c(rank, country, relocation_treatment))


rank_by_country %>% 
    filter(rank == 1, relocation_treatment == "No info") %>% 
    ggplot(
        aes(
            x = country, 
            y = percent, 
            fill = relocation,
            label = sprintf("%.0f%%", 100 * percent)
        )
    ) +
    geom_col(position = position_dodge(.75), width = .7) + 
    geom_text(position = position_dodge(.75), hjust = 1)  +
    scale_fill_brewer(palette = "Set1") +
    coord_flip()




plot_ranks <- rank_by_country %>% 
    mutate(percent = n / sum(n), .by = c(rank, country, relocation_treatment)) %>%
    filter(rank == 1, relocation_treatment == "No info") %>%
    mutate(
        ord = percent[relocation == relocation[1]],
        .by = country
    ) %>%
    ggplot(
        aes(
            x = percent,
            y = reorder(country, ord),
            fill = relocation, 
            label = sprintf("%.0f%%", 100 * percent)
        )
    ) + 
    geom_col() + 
    geom_text(
        position = position_stack(.5),
        aes(color = relocation != "No relocation"), 
        show.legend = FALSE
    ) +
    scale_color_manual(values = c("gray25", "white")) + 
    scale_fill_brewer(palette = "Blues", direction = 1) +
    facet_grid(country != "All" ~ relocation, scales = "free", space = "free")+
    theme(
        legend.position = "none",
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        strip.text.y = element_blank(),
        strip.text.x = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold"),
    ) + 
    labs(
        caption = "Source: JRC Fairness Migration Survey"
    )


fig_dir <- file.path("results", "figures")

filename <- file.path(fig_dir, "plot_ranks.png")
ggsave(filename, dpi = 600, width= 7, height = 3.5, bg = "white")
system(paste("open", filename))

# ----------------------------------------------------
# 3.1.2 Average rank
# ----------------------------------------------------

fit <- lm(rank ~ alt, data = ds_long)
summary(fit)
