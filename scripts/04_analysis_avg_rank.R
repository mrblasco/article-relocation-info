suppressMessages({
    library(dplyr)
    library(ggplot2)
    library(brms)
    library(broom.mixed)
})

source(file.path("R", "helpers.R"))

# --- load
ds_asylum_applications <- file.path("data", "processed", "fairness_survey_long.rds") |>
    read_rds()

# ---- analysis
model_avg_rank <- as.numeric(rank) ~ alt * treatment * country_type

fit_avg_rank <- fit_model(
    formula = model_avg_rank,
    data = ds_asylum_applications,
    family = gaussian()
)

model2 <- as.numeric(rank) ~ alt * treatment * country_type +
    treatment:asylum_applications_z +
    asylum_applications_z

fit_avg_rank__asylum_applications <- fit_model(
    formula = model2,
    data = ds_asylum_applications,
    family = gaussian()
)


# ---- plots
p_avg_rank <- fit_avg_rank |>
    extract_conditional_effects(
        effects = "treatment:alt",
        conditions = data.frame(
            country_type = c("Net receiver", "Net sender")
        )
    ) |>
    mutate(
        treatment = factor(
            treatment,
            c("Control", "Relative", "Absolute")
        )
    ) |>
    ggplot(
        aes(
            x = treatment,
            y = estimate__,
            ymin = lower__,
            ymax = upper__,
            group = alt,
            color = alt,
            shape = alt,
        )
    ) +
    geom_point(
        size = 4,
        position = position_dodge(0.2)
    ) +
    geom_errorbar(
        position = position_dodge(0.2)
    ) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = alt),
        alpha = 0.2, color = NA,
        position = position_dodge(width = 0.2)
    ) +
    facet_wrap(~country_type) +
    theme(
        legend.position = "bottom",
        legend.title = element_blank()
    ) +
    labs(
        x = "Information treatment",
        y = "Fairness rank (average)"
    ) +
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2")


save_plot("avg_rank.png", dpi = 300, width = 7, height = 4)


fit_avg_rank__asylum_applications %>%
    tidy(conf.int = TRUE) %>%
    filter(grepl("asyl", term)) %>%
    ggplot(
        aes(
            x = estimate,
            y = term,
            xmin = conf.low,
            xmax = conf.high
        )
    ) +
    geom_vline(xintercept = 0) +
    geom_pointrange()

fit_avg_rank__asylum_applications %>%
    extract_conditional_effects(
        effects = "treatment",
        conditions = expand.grid(
            alt = c("no_relocation", "relocation_GDP", "relocation_population"),
            country_type = c("Net sender"),
            asylum_applications_z = seq(-2, 2, by = .5)
        )
    ) %>%
    ggplot(
        aes(
            x = asylum_applications_z,
            y = estimate__,
            ymin = lower__,
            ymax = upper__,
            group = treatment,
            color = treatment,
            shape = treatment,
        )
    ) +
    geom_point(
        size = 4,
        position = position_dodge(0.2)
    ) +
    geom_errorbar(
        position = position_dodge(0.2)
    ) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = treatment),
        alpha = 0.2, color = NA,
        position = position_dodge(width = 0.2)
    ) +
    facet_grid(~alt) +
    theme(
        legend.position = "bottom",
        legend.title = element_blank()
    ) +
    labs(
        x = "Information treatment",
        y = "Fairness rank (average)"
    ) +
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2")


fit_top_rank <- lm(
    formula = rank == 1 ~ alt * treatment * country_type,
    data = ds_asylum_applications
)

fit_second <- lm(
    formula = rank == 2 ~ alt * treatment * country_type,
    data = ds_asylum_applications,
    subset = rank != 1
)

stargazer::stargazer(
    type = "text",
    fit_top_rank,
    fit_second,
    keep.stat = "n"
)

ds_asylum_applications |>
    distinct(alt, treatment, asylum_applications) |>
    count(alt, treatment)

ds_asylum_applications |>
    group_by(asylum_applications, alt) |>
    reframe(
        tidy(lm(as.numeric(rank) ~ treatment), conf.int = T)
    ) |>
    filter(
        grepl("treat", term)
    ) |>
    ggplot(
        aes(
            x = asylum_applications,
            y = estimate,
            ymax = conf.high,
            ymin = conf.low,
            color = alt,
            shape = alt
        )
    ) +
    facet_wrap(~term) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_pointrange(
        size = 1
    )


fit0 <- fit_model(
    as.numeric(rank) ~ treatment * alt + (treatment | asylum_applications),
    data = ds_asylum_applications,
    backend = "cmdstanr"
)


fit1 <- fit_model(
    rank == 1 ~ alt * treatment * country_type * asylum_applications_z + (1 | respondent_id),
    family = bernoulli(),
    data = ds_asylum_applications
)

fit2 <- fit_model(
    rank == 2 ~ alt * treatment * country_type + (1 | respondent_id),
    family = bernoulli(),
    data = subset(ds_asylum_applications, rank != 1)
)

# Posterios
newdata_control <- expand.grid(
    alt = unique(ds_asylum_applications$alt),
    treatment = "Control",
    country_type = unique(ds_asylum_applications$country_type),
    respondent_id = 12
)

newdata_abs <- newdata_control
newdata_abs$treatment <- "Absolute"

newdata_rel <- newdata_control
newdata_rel$treatment <- "Relative"

# - predictions
p_control <- posterior_epred(fit1, newdata = newdata_control, re.form = ~0)
p_abs <- posterior_epred(fit1, newdata = newdata_abs, re.form = ~0)
p_rel <- posterior_epred(fit1, newdata = newdata_rel, re.form = ~0)

p2_control <- posterior_epred(fit2, newdata = newdata_control, re.form = ~0)
p2_abs <- posterior_epred(fit2, newdata = newdata_abs, re.form = ~0)
p2_rel <- posterior_epred(fit2, newdata = newdata_rel, re.form = ~0)

p3_control <- 1 - p_control - p2_control
p3_abs <- 1 - p_abs - p2_abs
p3_rel <- 1 - p_rel - p2_rel

# ---- average rank
avg_control <- p_control + 2 * p2_control + 3 * p3_control
avg_abs <- p_abs + 2 * p2_abs + 3 * p3_abs
avg_rel <- p_rel + 2 * p2_rel + 3 * p3_rel

ate_abs <- avg_control - avg_abs
ate_rel <- avg_control - avg_rel


alpha <- 0.05

estimate <- apply(ate_abs, 2, quantile, p = c(alpha / 2, 0.5, 1 - alpha / 2))
est_abs <- data.frame(estimate = estimate[2, ], conf.low = estimate[1, ], conf.high = estimate[3, ])

estimate <- apply(ate_rel, 2, quantile, p = c(alpha / 2, 0.5, 1 - alpha / 2))
est_rel <- data.frame(estimate = estimate[2, ], conf.low = estimate[1, ], conf.high = estimate[3, ])


ds_ate <- bind_rows(
    bind_cols(newdata_abs, est_abs),
    bind_cols(newdata_rel, est_rel)
)


ds_ate |>
    ggplot(
        aes(
            x = estimate,
            y = alt,
            fill = country_type,
            label = sprintf("%2.2f", estimate)
        )
    ) +
    facet_grid(treatment ~ country_type) +
    geom_col() +
    geom_errorbar(
        aes(xmin = conf.low, xmax = conf.high),
        width = 0.2,
        color = "gray"
    ) +
    geom_text()


ds_ate %>%
    ggplot(
        aes(
            x = estimate,
            y = alt,
            colour = country_type
        )
    ) +
    geom_vline(
        xintercept = 0,
        linetype = 2,
        colour = "grey60",
        linewidth = 0.5
    ) +
    geom_errorbarh(
        aes(xmin = conf.low, xmax = conf.high),
        height = 0.15,
        linewidth = 0.7
    ) +
    geom_point(
        size = 2.8
    ) +
    facet_wrap(
        ~treatment,
        nrow = 1
    ) +
    scale_colour_brewer(
        palette = "Dark2",
        name = "Country type"
    ) +
    labs(
        x = "Average treatment effect on expected rank",
        y = NULL
    ) +
    theme_bw(base_size = 12) +
    theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "top"
    )
