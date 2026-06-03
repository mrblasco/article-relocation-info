suppressMessages({
    library(dplyr)
    library(purrr)
    library(tidyr)
    library(brms)
    library(broom.mixed)
    library(kableExtra)
    library(ggplot2)
    library(ggtext)
    library(here)
})


config <- yaml::read_yaml(
    here("config", "config.yml")
)

print(config)

knitr::opts_chunk$set(
    echo = FALSE,
    error = TRUE
)

source("R/helpers_data.R")
source("R/helpers_models.R")
source("R/helpers_plots.R")
source("R/logger.R")
source("R/theme.R")

theme_set(theme_nice())

create_dir <- function(path) {
    dir.create(path = path, showWarnings = FALSE, recursive = TRUE)
}

create_dir(config$output_figures)
create_dir(config$output_models)
set.seed(config$seed)

# ---- load data -----------------------

ds_survey <- read_rds(config$data_dir, "fair_survey_clean.rds")

tbl_asylum_rel <- data.frame(
    country = c(
        "Germany", "Spain", "Greece", "Bulgaria",
        "France", "Sweden", "Italy", "Poland"
    ),
    no_relocation = c(313, 138, 397, 178, 151, 344, 128, 16),
    population = c(169, 169, 169, 169, 169, 169, 169, 169),
    gdp = c(219, 136, 95, 65, 184, 230, 158, 92)
)

# ---- data processing ----

ds_long <- ds_survey %>%
    mutate(respondent_id = seq_len(n())) %>%
    pivot_longer(
        cols = c(
            no_relocation_ranking,
            relocation_population_ranking,
            relocation_GDP_ranking
        ),
        names_to = "alt",
        names_pattern = "(.*)_ranking",
        values_to = "rank",
    ) %>%
    select(
        respondent_id,
        alt,
        rank,
        country,
        treatment = relocation_treatment
    ) %>%
    mutate(
        rank = as.numeric(gsub("[^0-9]", "", rank)),
        country = factor(country),
        treatment = factor(treatment),
        alt = factor(alt)
    ) %>%
    glimpse()

ds_asylum_applications <- tbl_asylum_rel %>%
    mutate(
        country_type = case_when(
            no_relocation > population ~ "Net sender",
            no_relocation < population ~ "Net receiver",
            TRUE ~ "Other"
        )
    ) %>%
    pivot_longer(
        c("population", "gdp", "no_relocation"),
        names_to = "alt",
        values_to = "asylum_applications"
    ) %>%
    mutate(
        alt = replace_values(
            alt,
            "gdp" ~ "relocation_GDP",
            "population" ~ "relocation_population",
            "no_relocation" ~ "No relocation"
        )
    ) %>%
    right_join(
        ds_long,
        by = c("alt", "country")
    ) %>%
    mutate(
        asylum_applications_z = scale(asylum_applications)[, 1],
        rank = factor(rank, ordered = TRUE)
    )

# ----- model

model_formula <- rank ~ alt +
    treatment +
    asylum_applications_z +
    treatment:asylum_applications_z +
    country

m0 <- MASS::polr(
    formula = model_formula,
    data = ds_asylum_applications
)
summary(m0)

m0_coeffs <- tidy(m0, conf.int = TRUE)

m0_coeffs <- m0_coeffs %>%
    mutate(
        OR = exp(estimate),
        OR_low = exp(conf.low),
        OR_high = exp(conf.high),
        significant = !(conf.low <= 0 & conf.high >= 0),
        term = reorder(term, OR)
    )

ref_terms <- tibble(
    term = c("treatment (reference)", "country (reference categories)"),
    OR = 1,
    OR_low = 1,
    OR_high = 1,
    significant = FALSE
)

plot_data <- bind_rows(m0_coeffs, ref_terms)

plot_data %>%
    ggplot(
        aes(
            y = term,
            x = OR,
        )
    ) +
    geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.4, color = "grey50") +
    geom_errorbarh(
        aes(xmin = OR_low, xmax = OR_high, color = significant),
        height = 0.2,
        linewidth = 0.7
    ) +

    # point estimates
    geom_point(
        aes(color = significant),
        size = 2.8
    ) +

    # emphasize interaction term slightly
    geom_point(
        data = ~ filter(.x, grepl("treatment:asylum_applications_z", term)),
        size = 3.5,
        shape = 21,
        fill = "black",
        color = "black"
    ) +
    scale_color_manual(
        values = c("TRUE" = "#1f77b4", "FALSE" = "grey70"),
        guide = "none"
    ) +
    scale_x_log10() +
    labs(
        x = "Odds ratio (log scale)",
        y = NULL,
        title = "Determinants of ranking outcomes",
        subtitle = "Ordered logit model with interaction effect"
    ) +
    annotate("text", x = Inf, y = Inf, label = "Ordered logit model", hjust = 1.1)

ggplot(plot_data, aes(x = OR, y = term)) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    geom_errorbarh(
        data = subset(plot_data, !grepl("reference", term)),
        aes(xmin = OR_low, xmax = OR_high, color = significant),
        height = 0.2
    ) +
    geom_point(
        aes(color = significant),
        size = 2.5
    ) +
    geom_text(
        data = subset(plot_data, grepl("reference", term)),
        aes(label = "REFERENCE"),
        color = "grey50",
        hjust = -0.1,
        size = 3
    ) +
    scale_x_log10() +
    theme_minimal()


p1 <- ggplot(m0_coeffs, aes(x = OR, y = term)) +
    geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.4, color = "grey50") +
    geom_errorbarh(
        aes(xmin = OR_low, xmax = OR_high, color = significant),
        height = 0.2,
        linewidth = 0.7
    ) +
    geom_point(aes(color = significant), size = 2.6) +

    # highlight interaction term
    geom_point(
        data = subset(m0_coeffs, grepl("treatment:asylum_applications_z", term)),
        size = 3.4,
        shape = 21,
        fill = "black",
        color = "black"
    ) +
    scale_x_log10() +
    scale_color_manual(
        values = c("TRUE" = "#2C7FB8", "FALSE" = "grey70"),
        guide = "none"
    ) +
    labs(
        title = "Determinants of ranking outcomes",
        subtitle = "Ordered logit model (odds ratios)",
        x = "Odds ratio (log scale)",
        y = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        axis.text.y = element_text(size = 10)
    )

p2 <- ggplot() +
    theme_void() +
    annotate(
        "text",
        x = 0,
        y = 1,
        hjust = 0,
        vjust = 1,
        size = 4,
        label = paste0(
            "Model specification:\n",
            "rank ~ alt + treatment + asylum_applications_z + interaction + country FE\n\n",
            "Reference categories:\n",
            "- treatment: control group (baseline)\n",
            "- country: fixed effects included (omitted from plot)\n\n",
            "Notes:\n",
            "- Coefficients shown as odds ratios\n",
            "- 95% confidence intervals\n",
            "- Interaction term highlighted in black"
        )
    ) +
    xlim(0, 1) +
    ylim(0, 1)

p2

m0_coeffs %>% 
    filter(!grepl("country", term)) %>%
    ggplot(aes(x = OR, y = term)) +
    geom_vline(
        xintercept = 1, linetype = "dashed",
        linewidth = 0.35, color = "grey60"
    ) +

    # all coefficients (neutral grey)
    geom_errorbarh(
        aes(xmin = OR_low, xmax = OR_high),
        height = 0.15,
        color = "grey55",
        linewidth = 0.5
    ) +
    geom_point(
        color = "grey30",
        size = 1.9
    ) +

    # interaction highlighted (Nature-style single accent)
    geom_point(
        data = subset(m0_coeffs, grepl("treatment:asylum_applications_z", term)),
        aes(x = OR, y = term),
        color = "#D55E00",
        size = 2.6
    ) +
    scale_x_log10() +
    labs(
        x = "Odds ratio",
        y = NULL,
        title = "Determinants of ranking outcomes"
    ) +
    theme_classic(base_size = 11) +
    theme(
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(face = "bold"),
        axis.text.y = element_text(size = 9),
        plot.margin = margin(6, 6, 6, 6)
    )

# ------ old -------

ds_rank <- ds_asylum_applications %>%
    rename(
        alt = relocation,
        treatment = relocation_treatment,
    ) %>%
    mutate(
        treatment = factor(
            treatment,
            c("No info", "Relative", "Absolute")
        ),
        age_z = scale(age)[, 1],
        trust_eu_z = scale(trust_eu)[, 1],
        asylum_applications_z = scale(asylum_applications)[, 1],
        political_right_z = scale(political_right)[, 1],
        political_right_3 = cut(
            political_right,
            c(0, 3, 6, 10),
            include.lowest = TRUE,
            labels = c("Left", "Neutral", "Right")
        ),
        trust_eu_3 = cut(
            trust_eu,
            c(0, 3, 6, 10),
            include.lowest = TRUE,
            c("Low", "Middle", "High")
        )
    )


# ------ Main model ------------------------

model_formula <- rank ~ alt +
    treatment +
    asylum_applications_z +
    treatment:asylum_applications_z +
    country +
    (1 | respondent_id)

fit <- ds_rank |>
    fit_brm(
        formula = model_formula,
        cores = config$cores,
        file = here::here(
            config$output_models, "m_rank.rds"
        ),
        file_refit = "on_change",
    )


p <- tidy(fit) %>%
    filter(effect == "fixed", !grepl("Intercept|country", term)) %>%
    replace_terms() %>%
    bind_rows(
        data.frame(
            term = c("Relocation by GDP", "No info"),
            grp = c("Alternative", "Treatment"),
            estimate = 0
        )
    ) %>%
    mutate(
        term = factor(
            term,
            c(
                "Relocation by GDP",
                "Relocation by population",
                "No relocation",
                "Asylum applications (z)",
                "Absolute",
                "Relative",
                "No info",
                "Relative x asylum applications",
                "Absolute x asylum applications"
            )
        )
    ) %>%
    ggplot(
        aes(
            x = estimate,
            y = term,
            xmin = conf.low,
            xmax = conf.high,
            shape = ifelse(estimate == 0, "Omitted", "Category"),
            color = ifelse(estimate == 0, "Omitted", "Category")
        )
    ) +
    scale_color_manual(
        values = config$palette
    ) +
    geom_vline(
        xintercept = 0, linetype = "dashed", color = "grey60"
    ) +
    geom_pointrange() +
    labs(
        x = "Effect on fairness rankings\n(higher = perceived as less fair)",
        y = NULL
    )


save_plot(
    here(config$output_figures, "main_coef.pdf"),
    plot = p,
    formats = c("png", "pdf")
)

# ----

ds_grid <- expand_grid(
    asylum_applications_z = seq(
        min(ds_rank$asylum_applications_z, na.rm = TRUE),
        max(ds_rank$asylum_applications_z, na.rm = TRUE),
        length.out = 50
    ),
    treatment = unique(ds_rank$treatment),
    alt = unique(ds_rank$alt),
    country = "Germany"
)


p <- predict_model(fit, ds_grid) |>
    filter(alt %in% c("No relocation", "Population")) |>
    mutate(
        group = treatment,
        alt = replace_values(
            alt,
            "Population" ~ "Relocation\nby population"
        )
    ) |>
    ggplot(
        aes(
            x = asylum_applications_z,
            y = Estimate,
            color = group,
            fill = group,
            linetype = group
        )
    ) +
    geom_ribbon(
        aes(
            ymin = Q2.5,
            ymax = Q97.5
        ),
        alpha = .12,
        linewidth = 0
    ) +
    geom_line(linewidth = .9) +
    scale_color_manual(
        values = config$palette
    ) +
    scale_fill_manual(
        values = config$palette
    ) +
    labs(
        x = "Asylum applications (z-score)",
        y = "Predicted fairness ranking\n(higher = perceived as less fair)",
    ) +
    theme(
        legend.key.width = unit(1.8, "cm"),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(3, "lines"),
    ) +
    facet_grid(~alt)

save_plot(
    filename = here::here(config$output_figures, "asylum_applications.pdf"),
    width = 7, height = 4,
    plot = p
)

# ----

ce <- conditional_effects(
    fit,
    effects = "alt:asylum_applications_z",
    conditions = data.frame(
        treatment = c("No info", "Absolute")
    )
)

df_ce <- ce[[1]]

p <- df_ce %>%
    ggplot(
        aes(
            x = asylum_applications_z,
            y = estimate__,
            ymin = lower__,
            ymax = upper__,
            group = treatment,
            color = treatment,
            shape = treatment
        )
    ) +
    facet_wrap(
        ~ factor(alt, c("No relocation", "Population", "GDP"))
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
    labs(
        x = "Asylum applications (z-score)",
        y = "Expected fairness rank\n(higher = perceived as less fair)"
    ) +
    scale_x_continuous(breaks = c(-1, 0, 1), labels = \(x) paste(x, "SD")) +
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    theme(
        panel.spacing = unit(3, "lines"),
    )

save_plot(
    filename = here::here(config$output_figures, "conditiona_effects.pdf"),
    plot = p,
    width = 7 * 1.5,
    height = 3.5 * 1.5
)

quit("no")


# ---- heterogeneity models (political + trust) ----

fit_political <- fit_brm_grouped(
    data = ds_rank,
    formula = model_formula,
    group_var = "political_right_3",
    file_prefix = here::here(
        config$output_models, "m_rank_political"
    )
)

fit_political <- fit_brm_grouped(
    data = ds_rank,
    formula = model_formula,
    group_var = "political_right_3",
    file_prefix = here::here(
        config$output_models, "m_rank_political"
    )
)

# ----

p <- fit_political %>%
    lapply(tidy) %>%
    bind_rows(.id = "political_right") %>%
    filter(
        !grepl("Intercept|country", term),
        effect == "fixed"
    ) %>%
    replace_terms() %>%
    ggplot(
        aes(
            x = estimate,
            xmin = conf.low,
            xmax = conf.high,
            y = term,
            shape = political_right,
            color = political_right,
        )
    ) +
    geom_vline(
        xintercept = 0
    ) +
    geom_pointrange(
        position = position_dodge(.55)
    ) +
    scale_color_manual(
        values = config$palette
    ) +
    facet_grid(grp ~ ., scales = "free", space = "free") +
    labs(
        x = "Effect on fairness rankings\n(higher = perceived as less fair)",
        y = NULL
    )

save_plot(
    filename = here::here(config$output_figures, "political_right.pdf"),
    plot = p
)



# ----- coefficients -------

coef_political <- map_dfr(fit_political, tidy, .id = "group") %>%
    replace_terms()

coef_trust <- map_dfr(fit_trust, tidy, .id = "group") %>%
    replace_terms()

# ----- plot coefficients ----

plot_coef(
    coef_political,
    group_levels = c("Left", "Neutral", "Right"),
    palette = c(
        "Left"   = "#0072B2",
        "Center" = "#666666",
        "Right"  = "#D55E00"
    )
)


# ---- predictions grid ----



ce_political <- predict_models(fit_political, grid, names(fit_political))
ce_trust <- predict_models(fit_trust, grid, names(fit_trust))



# ---- plot predictions
ce_political %>%
    mutate(
        group = factor(
            group,
            levels = c("left", "neutral", "right"),
            labels = c("Left", "Center", "Right")
        )
    ) %>%
    ggplot(
        aes(
            x = asylum_applications_z,
            y = Estimate,
            color = group,
            fill = group,
            linetype = group
        )
    ) +
    geom_ribbon(
        aes(
            ymin = Q2.5,
            ymax = Q97.5
        ),
        alpha = .12,
        linewidth = 0
    ) +
    geom_line(linewidth = .9) +
    scale_color_manual(
        values = c(
            "Left"   = "#0072B2",
            "Center" = "#666666",
            "Right"  = "#D55E00"
        )
    ) +
    scale_fill_manual(
        values = c(
            "Left"   = "#0072B2",
            "Center" = "#666666",
            "Right"  = "#D55E00"
        )
    ) +
    labs(
        x = "Asylum applications (z-score)",
        y = "Predicted fairness ranking\n(higher = perceived as less fair)",
    ) +
    theme(
        legend.key.width = unit(1.8, "cm"),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(3, "lines"),
    ) +
    facet_grid(~ treatment + alt)

# ---- Trust in the EU --------------------------

trust_levels <- levels(ds_rank$trust_eu_3)

model <- rank ~ alt +
    treatment +
    asylum_applications_z +
    treatment:asylum_applications_z +
    (1 | country) +
    (1 | respondent_id)

fit_list <- list()

for (j in trust_levels) {
    fit_list[[j]] <- brm(
        formula = model,
        data = filter(ds_rank, trust_eu_3 == j),
        cores = 4,
        threads = threading(4),
        file = sprintf("m_rank_trust_%s.rds", j),
        file_refit = "on_change",
    )
}

coeffs <- lapply(fit_list, tidy) %>%
    bind_rows(.id = "group") %>%
    glimpse() %>%
    mutate(
        term = replace_values(
            term,
            "altPopulation" ~ "Relocation by population",
            "altNorelocation" ~ "No relocation",
            "treatmentRelative" ~ "Relative",
            "asylum_applications_z" ~ "Asylum applications (z)",
            "treatmentNoinfo" ~ "No info",
            "treatmentNoinfo:asylum_applications_z" ~
                "No info x asylum applications",
            "treatmentRelative:asylum_applications_z" ~
                "Relative x asylum applications"
        )
    )

coeffs %>%
    filter(effect == "fixed", !grepl("Intercept", term)) %>%
    mutate(
        group = factor(
            group,
            levels = c("Low", "Middle", "High")
        )
    ) %>%
    ggplot(
        aes(
            x = estimate,
            y = term,
            xmin = conf.low,
            xmax = conf.high,
            color = group,
            shape = group,
        )
    ) +
    geom_vline(
        xintercept = 0,
        linewidth = .4,
        linetype = "dashed",
        color = "grey55"
    ) +
    geom_pointrange(
        position = position_dodge(width = .55),
        linewidth = .5
    ) +
    scale_color_manual(
        values = c(
            "Low" = "#0072B2",
            "Middle" = "#666666",
            "High" = "#D55E00"
        )
    ) +
    labs(
        x = "Estimated effect on fairness ranking\n(higher = perceived as less fair)",
    )


ds_grid <- expand_grid(
    asylum_applications_z =
        seq(
            min(ds_rank$asylum_applications_z, na.rm = TRUE),
            max(ds_rank$asylum_applications_z, na.rm = TRUE),
            length.out = 50
        ),
    treatment = c("No info", "Absolute"),
    alt = unique(ds_rank$alt)[3] # keep fixed
)

ce <- map_dfr(
    names(fit_list),
    \(g) {
        fitted(
            fit_list[[g]],
            newdata = ds_grid,
            re_formula = NA,
            summary = TRUE
        ) |>
            as.data.frame() |>
            bind_cols(ds_grid) |>
            mutate(group = g)
    }
) %>% glimpse()


ce %>%
    mutate(
        group = factor(
            group,
            levels = c("Low", "Middle", "High")
        )
    ) %>%
    ggplot(
        aes(
            x = asylum_applications_z,
            y = Estimate,
            color = group,
            fill = group,
            linetype = group
        )
    ) +
    geom_ribbon(
        aes(
            ymin = Q2.5,
            ymax = Q97.5
        ),
        alpha = .12,
        linewidth = 0
    ) +
    geom_line(linewidth = .9) +
    scale_color_manual(
        values = c(
            "Low" = "#0072B2",
            "Middle" = "#666666",
            "High" = "#D55E00"
        )
    ) +
    scale_fill_manual(
        values = c(
            "Low" = "#0072B2",
            "Middle" = "#666666",
            "High" = "#D55E00"
        )
    ) +
    labs(
        x = "Asylum applications (z-score)",
        y = "Predicted fairness ranking\n(higher = perceived as less fair)",
    ) +
    theme(
        legend.key.width = unit(1.8, "cm"),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(3, "lines"),
    ) +
    facet_grid(~ treatment + alt)


# ---- Konowledge of the Asylum System --------------

# TBA

# ----- Fair share -------

# TBA

# ---- Age -----------------------------------

# TBA
