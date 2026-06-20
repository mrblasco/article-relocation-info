suppressMessages({
    library(dplyr)
    library(ggplot2)
})

# --- load 
filename <- here::here(
    "data", "processed",
    "fairness_survey_long.rds"
)
ds_asylum_applications <- readRDS(filename)
dplyr::glimpse(ds_asylum_applications)

params <- yaml::read_yaml(here::here("config", "_config.yml"))

# ---- helper functions
source(here::here("R", "helpers.R"))

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
    ggrepel::geom_text_repel(
        aes(
            label = sprintf(
                "%2.0f%%",
                100 * estimate__
            )
        ),
        direction = "x",
        vjust = -0.5,
        size = 4,
        color = "gray25"
    ) +
    scale_x_continuous(
        labels = scales::percent
    ) +
    scale_color_manual(
        values = params$palette
    ) +
    labs(
        x = "Respondents (%)",
        y = NULL
    ) +
    facet_grid(cats__ ~ country_type, switch = "both") +
    theme(
        legend.position = "none",
        panel.grid.major = element_line(
            linetype = "dashed",
            linewidth = 0.25
        ),
        panel.spacing = unit(2, "lines"),
        strip.placement = "outside"
    )

save_plot("top_rank_by_cntry_type.png", dpi = 300)

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
    ggrepel::geom_text_repel(
        aes(
            label = sprintf(
                "%2.0f%%",
                100 * estimate__
            )
        ),
        direction = "x",
        vjust = -0.5,
        size = 4,
        color = "gray25"
    ) +
    scale_x_continuous(
        labels = scales::percent
    ) +
    scale_color_manual(
        values = params$palette
    ) +
    labs(
        x = "Respondents (%)",
        y = NULL
    ) +
    facet_grid(country ~ cats__, switch = "both") +
    theme(
        legend.position = "none",
        panel.grid.major = element_line(
            linetype = "dashed",
            linewidth = 0.25
        ),
        panel.spacing = unit(2, "lines"),
        strip.placement = "outside"
    )

save_plot("top_rank_by_cntry.png", dpi = 300)
