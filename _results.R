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
options(vsc.dev.args = list(width = 800, height = 500))

knitr::opts_chunk$set(echo = FALSE, error = TRUE)

source("R/helpers_data.R")
source("R/helpers_models.R")
source("R/helpers_plots.R")
source("R/logger.R")
source("R/theme.R")

theme_set(theme_nice())

config <- yaml::read_yaml(here("config", "config.yml"))

dir.create(config$output_figures, showWarnings = FALSE, recursive = TRUE)
dir.create(config$output_models, showWarnings = FALSE, recursive = TRUE)
set.seed(config$seed)

# ---- load data -----------------------

ds_survey <- read_rds(config$data_dir, "fair_survey_clean.rds")
ds_long_raw <- read_rds(config$data_dir, "fair_survey_long.rds")

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

ds_long <- ds_long_raw %>%
    left_join(
        tbl_asylum_rel,
        by = "country"
    ) %>%
    mutate(
        country_label = case_when(
            no_relocation > population ~ "Net sender",
            no_relocation < population ~ "Net receiver",
            TRUE ~ "Other"
        )
    ) %>%
    select(-alt) %>%
    recode_labels()

ds_asylum_applications <- tbl_asylum_rel %>%
    tidyr::pivot_longer(
        c("population", "gdp", "no_relocation"),
        names_to = "relocation",
        values_to = "asylum_applications"
    ) %>%
    mutate(
        relocation = recode_values(
            relocation,
            "gdp" ~ "GDP",
            "population" ~ "Population",
            "no_relocation" ~ "No relocation"
        )
    ) %>%
    right_join(
        ds_long,
        by = c("relocation", "country")
    )

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

#    plot_coef_base(palette = config$palette, title = "Effect")

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
    facet_grid(~ alt)

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
    )+
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
