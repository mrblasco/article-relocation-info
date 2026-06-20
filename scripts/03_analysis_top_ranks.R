suppressMessages({
    library(dplyr)
    library(ggplot2)
})

source("R/helpers.R")
source("R/theme.R")

theme_set(theme_nice())

# --- load data
filename <- here::here(
    "data", "processed",
    "fairness_survey_long.rds"
)
ds_asylum_applications <- readRDS(filename)

params <- yaml::read_yaml(here::here("config", "_config.yml"))

# ---- analysis
ds_multinomial <- ds_asylum_applications |>
    dplyr::filter(rank == 1)

fit_top_rank <- fit_model(
    formula = alt ~ treatment * country_type,
    family = brms::categorical(),
    data = ds_multinomial
)

fit_top_rank_by_cntry <- fit_model(
    formula = alt ~ treatment * country,
    family = brms::categorical(),
    data = ds_multinomial
)

# ---- plots
p_top_rank <- fit_top_rank |>
    extract_conditional_effects(
        categorical = TRUE,
        conditions = expand.grid(
            country_type = unique(ds_multinomial$country_type)
        )
    ) |>
    mutate(
        cats__ = factor(
            cats__,
            c("no_relocation", "relocation_population", "relocation_GDP"),
            c("No relocation", "Relocation\nby population", "Relocation\nby GDP")
        ),
        treatment = factor(
            treatment,
            c("Control", "Relative", "Absolute")
        )
    ) |>
    ggplot(aes(
        x = estimate__,
        y = treatment,
        color = effect2__,
        xmin = lower__,
        xmax = upper__
    )) +
    geom_pointrange() +
    geom_label(
        border.color = NA,
        hjust = 1,
        vjust = 0.5,
        aes(
            x = Inf,
            label = sprintf(
                "%2.0f%%",
                100 * estimate__
            )
        ),
        size = 3.5,
        color = "gray25"
    ) +
    scale_x_continuous(
        limits = c(0.1, 0.51),
        labels = scales::percent
    ) +
    scale_color_manual(
        values = params$palette
    ) +
    labs(
        x = "Respondents per country type (%)",
        y = NULL
    ) +
    facet_grid(cats__ ~ country_type, switch = "y") +
    theme(
        legend.position = "none",
        panel.grid.major = element_line(
            linetype = "dashed",
            linewidth = 0.25
        ),
        panel.spacing = unit(2, "lines"),
        strip.placement = "outside"
    )

img_path <- "output/figures/top_rank_by_cntry_type.png"
ggsave(img_path,  dpi = 300)
ggsave(gsub("png$", "pdf", img_path))

if (interactive()) system(paste("open", img_path))


p_top_rank_by_cntry <- fit_top_rank_by_cntry |>
    extract_conditional_effects(
        categorical = TRUE,
        plot = FALSE,
        conditions = expand.grid(
            country = unique(ds_multinomial$country)
        )
    ) |>
    mutate(
        cats__ = factor(
            cats__,
            c("no_relocation", "relocation_population", "relocation_GDP"),
            c("No relocation", "Relocation\nby population", "Relocation\nby GDP")
        ),
        treatment = factor(
            treatment,
            c("Control", "Relative", "Absolute")
        )
    ) |>
    ggplot(aes(
        x = estimate__,
        y = treatment,
        color = effect2__,
        xmin = lower__,
        xmax = upper__
    )) +
    geom_pointrange() +
    geom_label(
        border.color = NA,
        hjust = 1,
        vjust = 0.5,
        aes(
            x = Inf,
            label = sprintf(
                "%2.0f%%",
                100 * estimate__
            )
        ),
        size = 3.5,
        color = "gray25"
    ) +
    scale_x_continuous(
        limits = c(0, 1),
        breaks = c(0, .5, 1),
        labels = scales::percent
    ) +
    scale_color_manual(
        values = params$palette
    ) +
    labs(
        x = "Respondents (%)",
        y = NULL
    ) +
    facet_grid(country ~ cats__, switch = "y") +
    theme(
        legend.position = "none",
        panel.grid.major = element_line(
            linetype = "dashed",
            linewidth = 0.25
        ),
        panel.spacing = unit(2, "lines"),
        strip.placement = "outside"
    )

img_path <- "output/figures/top_rank_by_cntry.png"
ggsave(img_path,  dpi = 300, height = 10, width = 7)
ggsave(gsub("png$", "pdf", img_path), height = 10, width = 7)

if (interactive()) system(paste("open", img_path))
