# ----------------------------------------------------
# Libraries
# ----------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)

theme_set(theme_minimal(base_size = 12))


# ----------------------------------------------------
# Paths
# ----------------------------------------------------

fig_dir <- file.path("results", "figures")
dir.create(fig_dir)

data_dir <- file.path("data", "processed")
ds_survey_filename <- file.path(data_dir, "fair_survey_clean.rds")
ds_long_filename <- file.path(data_dir, "fair_survey_long.rds")

# ----------------------------------------------------
# Load data
# ----------------------------------------------------

ds_long <- readRDS(ds_long_filename)
ds_survey <- readRDS(ds_survey_filename)

# ----------------------------------------------------
# Perceptions under no information
# ----------------------------------------------------

ds_long_count <- ds_long %>% 
    bind_rows(mutate(., country = "All")) %>%

    count(relocation_treatment, relocation, rank, country) %>%
    mutate(percent = 100 * n / sum(n),
         .by = c(country, rank, relocation_treatment))

# ----------------------------------------------------
# Statistical association
# ----------------------------------------------------

xtabs(~  relocation_treatment + relocation + rank, data = ds_long) %>% 
    apply("rank", function(x) chisq.test(x)$p.value)

xtabs(~  relocation_treatment + relocation + country, data = ds_long, subset = rank == 1) %>% 
    apply("country", function(x) chisq.test(x)$p.value) %>% 
    p.adjust() %>% 
    sprintf('%.2f', .)


# ----------------------------------------------------
# Fig.1 Fairness mechanisms under no information
# ----------------------------------------------------

ds_top_rank <- ds_long_count %>%   
    mutate(all = country == "All") %>% 
    filter(rank == 1, relocation_treatment == "No info") 

ds_obs <- ds_top_rank %>% 
    summarise(n_obs = sum(n), .by = country) %>% 
    mutate(country_lab = sprintf("<strong>%s</strong> (n=%d)", country, n_obs))

p_base <- ds_top_rank %>%
    left_join(ds_obs, by = "country") %>% 
    mutate(
        mean_group = percent[relocation == relocation[1]],
        .by = country
    ) %>%
    ggplot(
        aes(
            x = percent,
            y = reorder(country_lab, mean_group),
            fill = relocation,
            label = sprintf("%2.0f%%", percent),
            color = grepl("by pop", relocation)
        )
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_color_manual(values = c("black", "white")) +
    scale_fill_brewer(palette = "Blues") +
    facet_grid(all ~ relocation, scales = "free_y", space = "free_y") +
    geom_col(color = NA) +
    geom_text(position = position_stack(.5), show.legend = FALSE) + 
    theme(
        legend.position = "none",
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.text = ggtext::element_markdown(),
        strip.text.y = element_blank(),
        strip.text.x = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5),
    ) + 
    labs(
        title = "Fairest Allocation Mechanism"
    )


ggsave(
    file.path(fig_dir, "top_rank_no_info.png"), 
    p_base,
    dpi = 600, bg = "white", width = 7, height = 3.5
)

saveRDS(p_base, file.path(fig_dir, "top_rank_no_info.rds"))

# ----------------------------------------------------
# Fig.2 Treatment differences
# ----------------------------------------------------

ds_asylum_rel <- data.frame(
  country = c("Germany", "Spain", "Greece", "Bulgaria",
              "France", "Sweden", "Italy", "Poland"),
  no_relocation = c(313, 138, 397, 178, 151, 344, 128, 16),
  population = c(169, 169, 169, 169, 169, 169, 169, 169),
  gdp = c(219, 136, 95, 65, 184, 230, 158, 92)
) 

ds_mean_rank <- ds_long %>% 
    reframe(
        {
            broom::tidy(lm(rank ~ relocation_treatment), conf.int = TRUE)
        },
        .by = c(country, relocation)
    )

col_width <- 0.7

ds_plot <- ds_mean_rank %>%
    filter(term != "(Intercept)") %>% 
    left_join(ds_asylum_rel, by = "country")

p_mean_rank <- ds_plot %>% 
    ggplot(
        aes(
            x = estimate,
            xmin = conf.low, 
            xmax = conf.high,
            fill = term, 
            y = term,
            alpha = abs(estimate) > 2 * std.error
        )
    ) +
    facet_grid(country ~ relocation) + 
    geom_vline(xintercept = 0, linetype = 2, color = "red") +
    geom_col(
        position = position_dodge(col_width),
        width = col_width
    ) + 
    geom_errorbar(
        position = position_dodge(col_width),
        width = 0.2
    ) + 
    geom_point() +
    scale_fill_brewer(palette = "Blues", name = "Treatment") + 
    scale_y_discrete(labels = function(x) gsub("relocation_treatment", "", x)) +
    scale_x_continuous(limits = c(-1, 1) * 0.7) +
    scale_alpha_manual(values = c(0.2, 1)) + 
    theme(
        legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title = element_blank(),
        strip.text.x = ggtext::element_markdown(lineheight = 1.2),
        panel.spacing = unit(.1, "lines"),
        panel.background = element_rect(color = "gray75"),
    ) +
    labs(
        title = "Mean Fairness Rank Difference",
        subtitle = "Baseline = Absolute Info"
    )


ggsave(
    file.path(fig_dir, "mean_rank.png"),
    p_mean_rank,
    dpi = 600, bg = "white", w = 7, h = 7
)
system(paste("open", file.path(fig_dir, "mean_rank.png")))

saveRDS(p_mean_rank, file.path(fig_dir, "mean_rank.rds"))
