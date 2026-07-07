# ------------------------------------------------------------
# Setup
# ------------------------------------------------------------
suppressMessages({
    library(dplyr)
    library(stringr)
    library(tidyr)
    library(brms)
    library(broom.mixed)
    library(kableExtra)
    library(ggplot2)
})

source("R/helpers.R")
source("R/theme.R")

params <- load_params()

set.seed(params$seed)
sapply(params$paths, dir.create, showWarnings = FALSE)

theme_set(theme_nice())

# ------------------------------------------------------------
# Load data
# ------------------------------------------------------------

tbl_asylum_rel <- params$asylum_relocation |>
    dplyr::bind_rows()

ds_raw <- read_rds(params$paths$raw_data)

ds_long <- ds_raw |>
    clean_data() |>
    pivot_data()

ds_asylum_applications <- tbl_asylum_rel |>
    tidyr::pivot_longer(
        c("population", "gdp", "no_relocation"),
        names_to = "alt",
        values_to = "asylum_applications"
    ) |>
    dplyr::mutate(
        alt = dplyr::replace_values(
            alt,
            "gdp" ~ "relocation_GDP",
            "population" ~ "relocation_population",
        )
    ) |>
    dplyr::right_join(
        ds_long,
        by = c("alt", "country")
    ) |>
    dplyr::mutate(
        asylum_applications_z = scale(asylum_applications)[, 1]
    )


# ------------------------------------------------------------
# 2.1 Methods
# ------------------------------------------------------------

# ------------------------------------------------------------
# Table 1. Asylum applications
# ------------------------------------------------------------
tab <- tbl_asylum_rel %>%
    dplyr::mutate(
        country = factor(
            country,
            levels = country[order(no_relocation, decreasing = TRUE)]
        ),
        d_pop = population - no_relocation,
        d_gdp = gdp - no_relocation
    ) %>%
    dplyr::arrange(
        dplyr::desc(no_relocation)
    ) %>%
    dplyr::mutate(
        Country = country,
        "No relocation" = no_relocation,
        "By population" = population,
        "By GDP" = gdp,
        `Δ Population` = d_pop,
        `Δ GDP` = d_gdp,
        .keep = "none"
    )


save_rds(tab, file.path(params$paths$tables, "table_asylum_relocation.rds"))


# ------------------------------------------------------------
# Table A1: Random assignment
# ------------------------------------------------------------
tab <- xtabs(~ country + treatment, data = ds_clean)
save_rds(tab, file.path(params$paths$tables, "table_country_treatment.rds"))

if (interactive()) {
    tab |>
        kableExtra::kbl(digits = 1) |>
        kableExtra::kable_classic()
}


# ------------------------------------------------------------
# Table 2: Summary and covariate balance
# ------------------------------------------------------------

df <- ds_asylum_applications |>
    dplyr::mutate(
        number_accepted_refugees = number_accepted_refugees / 1e3,
        male = sex == "Male",
        educ_high = ISCED == "High",
        educ_mid = ISCED == "Mid",
        educ_low = ISCED == "Low",
        dplyr::across(where(is.logical), as.numeric)
    ) |>
    dplyr::select(
        -matches(
            "current|applications|weight|restart_count|^t[DQ]|^tIn|respondent_id|underst",
        )
    )

numeric_vars <- names(df)[sapply(df, is.numeric)]

res <- lapply(numeric_vars, function(var) {
    means <- tapply(df[[var]], df$treatment, mean, na.rm = TRUE)
    sds <- tapply(df[[var]], df$treatment, sd, na.rm = TRUE)
    pooled_sd <- sd(df[[var]], na.rm = TRUE)
    smd_1 <- (means[1] - means[2]) / pooled_sd
    smd_2 <- (means[1] - means[3]) / pooled_sd
    smd_3 <- (means[2] - means[3]) / pooled_sd
    data.frame(
        treatment = names(means),
        estimate = means,
        SD = sds,
        max_smd = max(c(smd_1, smd_2, smd_3), na.rm = TRUE),
        row.names = NULL
    )
})
names(res) <- numeric_vars

tab <- res |>
    dplyr::bind_rows(.id = "variable") |>
    tidyr::pivot_wider(
        names_from = "treatment",
        names_vary = "slowest",
        values_from = c(estimate, SD)
    ) |>
    dplyr::mutate(
        max_smd = sprintf("%2.3f", max_smd)
    ) |>
    dplyr::mutate(
        variable = dplyr::recode(variable,
            age = "Age (years)",
            male = "Male (%)",
            educ_high = "High education (%)",
            educ_mid = "Mid education (%)",
            educ_low = "Low education (%)",
            trust = "Trust others (0-10)",
            trust_eu = "Trust in EU (0-10)",
            trust_gov = "Trust in government (0-10)",
            political_right = "Political right (0-10)",
            share_legal_migrants = "Share legal migrants",
            asylum_knowledge = "Knowledge of asylum system (1-4)",
            asylum_fraud_detected = "Fraud detection ability (0-10)",
            asylum_decisions_objective = "Asylum decisions are objective (0-10)",
            asylum_distr_even = "Asylums in EU are even (0-10)",
            fair_share = "Asylums in own country (1 = too few, 3 = too many)",
            burden_sharing_important = "Importance of burden sharing (0-10)",
            equal_distribution_important = "Importance of equal distribution (0-10)",
            equal_treatment_important = "Importance of equal treatment (0-10)",
            equal_rights_important = "Importance of equal rights (0-10)",
            quick_processing_important = "Importance of quick process (0-10)",
            Duration = "Completion time (mins)",
            number_accepted_refugees = "Accepted refugees in own country",
            .default = variable
        ),
        group = dplyr::case_when(
            variable %in% c(
                "Age (years)", "Male (%)",
                "High education (%)", "Mid education (%)", "Low education (%)"
            ) ~ "Sociodemographics",
            variable %in% c(
                "Trust others (0-10)", "Trust in EU (0-10)",
                "Trust in government (0-10)", "Political right (0-10)"
            ) ~ "Political attitudes",
            variable %in% c(
                "Knowledge of asylum system (1-4)",
                "Fraud detection ability (0-10)"
            ) ~ "Knowledge",
            variable %in% "Completion time (mins)" ~ "Survey administration",
            TRUE ~ "Other"
        )
    ) |>
    dplyr::arrange(desc(group)) |>
    dplyr::mutate(
        variable = stringr::str_replace(variable, "Importance of", "Value of"),
    )

save_rds(tab, file.path(params$paths$tables, "covariate_balance.rds"))

if (interactive()) {
    tab |>
        kableExtra::kbl(digits = 1) |>
        kableExtra::kable_classic()
}


# ------------------------------------------------------------
# 3.1 Perceived fairness
# ------------------------------------------------------------

ds_multinomial <- ds_asylum_applications |>
    dplyr::filter(rank == 1)

models <- list(
    m0 = alt ~ treatment * country_type,
    m1 = alt ~ treatment * country
)

fits <- lapply(models, fit_model, data = ds_multinomial, family = brms::categorical())

coeffs <- lapply(fits, broom.mixed::tidy) |>
    dplyr::bind_rows(.id = "model") |>
    dplyr::filter(
        grepl("treatment", term)
    ) |>
    dplyr::mutate(
        alt = case_when(
            str_detect(term, "murelocationGDP") ~ "GDP",
            str_detect(term, "murelocationpopulation") ~ "Population",
            TRUE ~ NA_character_
        ),
        treatment = case_when(
            str_detect(term, "_treatmentControl") ~ "Control",
            str_detect(term, "_treatmentRelative") ~ "Relative",
            TRUE ~ NA_character_
        ),
        modifier = case_when(
            str_detect(term, "country_typeNetsender") ~ "Net sender",
            str_detect(term, "countryFrance") ~ "France",
            str_detect(term, "countryGermany") ~ "Germany",
            str_detect(term, "countryGreece") ~ "Greece",
            str_detect(term, "countryItaly") ~ "Italy",
            str_detect(term, "countryPoland") ~ "Poland",
            str_detect(term, "countrySpain") ~ "Spain",
            str_detect(term, "countrySweden") ~ "Sweden",
            TRUE ~ NA_character_
        ),
        readable_term = case_when(
            is.na(modifier) ~ paste(alt, "-", treatment),
            TRUE ~ paste(alt, "-", treatment, "×", modifier)
        )
    )



# ------------------------------------------------------------
# Table A.2
# ------------------------------------------------------------
tab <- xtabs(~ treatment + alt + country_type, ds_multinomial)
save_rds(tab, file.path(params$paths$tables, "table_top_ranked_alternative.rds"))

# ------------------------------------------------------------
# Figure A1: Effect on top rank
# ------------------------------------------------------------
p <- ds_multinomial %>%
    bind_rows(., mutate(., current_residents = 1, weight = 1), .id = "with_weights") %>%
    count(alt, with_weights, wt = weight * current_residents) %>%
    mutate(percent = n / sum(n), .by = with_weights) %>%
    ggplot(
        aes(
            x = percent,
            y = alt,
            fill = alt
        )
    ) +
    facet_wrap(
        ~with_weights,
        ncol = 1,
        labeller = as_labeller(
            c(
                "1" = "Population-weighted sample",
                "2" = "Unweighted sample"
            )
        )
    ) +
    geom_col(
        width = 0.75
    ) +
    geom_text(
        position = position_stack(.5),
        color = "whitesmoke",
        aes(
            label = sprintf(
                "%2.0f%%",
                100 * percent
            )
        )
    ) +
    scale_y_discrete(
        name = NULL,
        labels = params$alt_labels
    ) +
    scale_x_continuous(
        name = "Respondents (%)",
        labels = scales::percent
    ) +
    scale_fill_manual(
        values = params$alt_palette
    ) +
    theme(
        panel.grid.major.y = element_blank(),
        legend.position = "none"
    )

if (interactive()) {
    print(p)
}

save_plot(
    file.path(params$paths$figures, "asylum-relocation-weighted-vs-unweighted.pdf"),
    width = 7,
    height = 5
)


# ------------------------------------------------------------
# Figure 1: Treatment Effect on top rankings
# ------------------------------------------------------------
p <- fits[[1]] |>
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
        color = treatment,
        xmin = lower__,
        xmax = upper__
    )) +
    geom_pointrange(
        size = 0.25
    ) +
    geom_label(
        border.color = NA,
        hjust = 0.5,
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
    coord_cartesian(clip = "off") +
    scale_x_continuous(
        limits = c(0.1, .55),
        breaks = c(0.25, 0.5),
        labels = scales::percent
    ) +
    scale_color_manual(
        values = params$treatment_palette
    ) +
    labs(
        x = "Respondents per country type (%)",
        y = NULL
    ) +
    facet_grid(country_type ~ cats__, switch = "y") +
    theme(
        legend.position = "none",
        panel.grid.major = element_line(
            linetype = "dashed",
            linewidth = 0.25
        ),
        panel.spacing = unit(2, "lines"),
        strip.placement = "outside"
    )

if (interactive()) {
    print(p)
}

file.path(params$paths$figures, "top_rank_by_cntry_type.png") |>
    save_plot(height = 3.5, width = 7)


# ------------------------------------------------------------
# Figure A3: Effect on top rank by country
# ------------------------------------------------------------
p <- fits[[2]] |>
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
        color = treatment,
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
        values = params$treatment_palette
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

file.path(params$paths$figures, "top_rank_by_cntry.png") |>
    save_plot(height = 10, width = 7)

# ------------------------------------------------------------
# 3.2 Averasge rankings
# ------------------------------------------------------------

models <- list(
    m0 = as.numeric(rank) ~ alt * treatment * country_type,
    m1 = as.numeric(rank) ~ alt * treatment * country,
    m2 = as.numeric(rank) ~ asylum_applications_z * treatment
)

fits <- lapply(models, lm, data = ds_asylum_applications)

fits_avg_ranks <- lapply(models, fit_model, data = ds_asylum_applications, family = gaussian())

# ------------------------------------------------------------
# Figure : Average ranks
# ------------------------------------------------------------
p <- fits_avg_ranks[[1]] |>
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
    geom_ribbon(
        aes(ymin = lower__, ymax = upper__, fill = alt),
        alpha = 0.2, color = NA
    ) +
    geom_point(
        size = 3
    ) +
    facet_wrap(~country_type) +
    theme(
        legend.position = "bottom",
        legend.title = element_blank()
    ) +
    scale_y_continuous(
        trans = "reverse"
    ) +
    scale_color_manual(
        values = params$alt_palette,
        labels = params$alt_labels,
    ) +
    scale_fill_manual(
        values = params$alt_palette,
        labels = params$alt_labels,
    ) +
    scale_shape_manual(
        values = c(no_relocation = 15, relocation_population = 16, relocation_GDP = 17),
        labels = params$alt_labels
    ) +
    labs(
        x = NULL,
        y = "Perceived fairness\n(reversed avg. rankings)"
    )
if (interactive()) p

save_plot(file.path(params$paths$figures, "avg_rank.png"), width = 7, height = 4.5)

# ------------------------------------------------------------
# Figure xXX: Effect on average rank
# ------------------------------------------------------------

p <- ds_asylum_applications |>
    ggplot(
        aes(
            y = as.numeric(rank),
            x = asylum_applications_z,
            color = treatment,
            linetype = treatment,
            fill = treatment
        )
    ) +
    geom_smooth(
        method = "lm",
        linewidth = 1
    ) +
    scale_y_continuous(
        trans = "reverse"
    ) +
    scale_color_manual(
        values = params$treatment_palette
    ) +
    scale_fill_manual(
        values = params$treatment_palette
    ) +
    theme(
        legend.key.width = unit(1.5, "cm"),
        legend.key.height = unit(.1, "cm")
    ) +
    labs(
        x = "Asylum applications (z-scored)",
        y = "Perceived fairness\n(reveresed avg. rankings)"
    )

if (interactive()) {
    print(p)
}

file.path(params$paths$figures, "avg_rank_asylum_applications_lm.pdf") |>
    save_plot(width = 7, height = 5)

# ------------------------------------------------------------
# 3.3 heterogeneity
# ------------------------------------------------------------

ds_anova <- ds_asylum_applications |>
    mutate(
        rank = as.numeric(rank),
        fair_share = factor(
            fair_share,
            c("No, the share is too low", "Yes", "No, the share is too high")
        )
    )

models <- list(
    # Trust EU
    m0 = rank ~ alt * treatment * country_type + trust_eu,
    m1 = rank ~ alt * treatment * country_type * trust_eu,
    # Political right
    m2 = rank ~ alt * treatment * country_type + political_right,
    m3 = rank ~ alt * treatment * country_type * political_right,
    # Age
    m4 = rank ~ alt * treatment * country_type + scale(age),
    m5 = rank ~ alt * treatment * country_type * scale(age),
    # asylum_knowledge
    m6 = rank ~ alt * treatment * country_type + asylum_knowledge,
    m7 = rank ~ alt * treatment * country_type * asylum_knowledge,
    # Trust EU
    m8 = rank ~ alt * treatment * country_type + fair_share,
    m9 = rank ~ alt * treatment * country_type * fair_share
)
fits <- lapply(models, lm, data = ds_anova)

comparisons <- list(
    "Trust in EU"      = c("m0", "m1"),
    "Political orientation"  = c("m2", "m3"),
    "Age"              = c("m4", "m5"),
    "Asylum knowledge" = c("m6", "m7"),
    "Fair share"        = c("m8", "m9")
)

anova_table <- purrr::imap_dfr(comparisons, function(mods, moderator) {
    base_mod <- fits[[mods[1]]]
    int_mod <- fits[[mods[2]]]

    a <- anova(base_mod, int_mod)

    tibble(
        Moderator = moderator,
        `Base model` = deparse(formula(base_mod)),
        `Interaction added` = paste0(
            "+ ",
            moderator,
            " four-way interaction"
        ),
        `ΔDf` = a$Df[2],
        `F` = round(a$F[2], 2),
        `p` = signif(a$`Pr(>F)`[2], 3),
        `ΔR²` = round(
            summary(int_mod)$r.squared -
                summary(base_mod)$r.squared,
            3
        )
    )
})

if (interactive()) {
    anova_table |>
        kableExtra::kbl() |>
        kableExtra::kable_classic()
}

save_rds(anova_table, file.path(params$paths$tables, "anova_table.rds"))


# ------------------------------------------------------------
#  Figure 4: Political orientation
# ------------------------------------------------------------

rank_style <- function(params, k = -1, breaks = c(0, 5, 10)) {
    list(
        geom_smooth(
            method = "gam",
            formula = y ~ s(x, bs = "cs", k = k),
            se = TRUE,
            linewidth = 0.9
        ),
        scale_y_continuous(transform = "reverse"),
        scale_x_continuous(breaks = breaks),
        scale_color_manual(values = params$treatment_palette),
        scale_fill_manual(values = params$treatment_palette),
        scale_linetype_manual(values = unlist(params$treatment_linetype))
    )
}

theme_update(
    legend.key.width = unit(1.5, "cm"),
    legend.key.height = unit(0.1, "cm")
)

p <- ds_asylum_applications |>
    ggplot(
        aes(
            x = political_right,
            y = as.numeric(rank),
            fill = treatment,
            color = treatment,
            linetype = treatment
        )
    ) +
    facet_grid(
        country_type ~ alt,
        labeller = labeller(alt = unlist(params$alt_labels))
    ) +
    rank_style(params) +
    labs(
        y = "Perceived fairness\n(reversed avg. ranking)",
        x = "Political orientation (0 = Left, 10 = Right)"
    )

if (interactive()) {
    print(p)
}

file.path(params$paths$figures, "political_right.png") |>
    save_plot(width = 9, height = 5)

# ------------------------------------------------------------
#  Figure 5: Fair share
# ------------------------------------------------------------
p <- ds_asylum_applications %>%
    mutate(
        fair_share = factor(
            fair_share,
            c("No, the share is too low", "Yes", "No, the share is too high")
        )
    ) %>%
    ggplot(
        aes(
            x = as.numeric(fair_share),
            y = as.numeric(rank),
            fill = treatment,
            color = treatment,
            linetype = treatment
        )
    ) +
    facet_grid(
        country_type ~ alt,
        labeller = labeller(alt = unlist(params$alt_labels))
    ) +
    rank_style(params, k = 3, breaks = 1:3) +
    labs(
        y = "Perceived fairness\n(reversed avg. ranking)",
        x = "Fair share of migrants in the EU (1 = Too low, 2 = Fair, 3 = Too high)"
    )

if (interactive()) {
    print(p)
}

file.path(params$paths$figures, "fair_share.png") |>
    save_plot(width = 9, height = 5)

# ------------------------------------------------------------
#  Figure 6: Age
# ------------------------------------------------------------
p <- ds_asylum_applications %>%
    mutate(
        age = ifelse(age < 80, age, 80)
    ) %>%
    ggplot(
        aes(
            x = as.numeric(age),
            y = as.numeric(rank),
            fill = treatment,
            color = treatment,
            linetype = treatment
        )
    ) +
    facet_grid(
        country_type ~ alt,
        labeller = labeller(alt = unlist(params$alt_labels))
    ) +
    rank_style(params, breaks = seq(0, 80, by = 20)) +
    labs(
        y = "Perceived fairness\n(reversed avg. ranking)",
        x = "Age (years)"
    )

if (interactive()) {
    print(p)
}

file.path(params$paths$figures, "age.png") |>
    save_plot(width = 9, height = 5)


# ------------------------------------------------------------
#  Figure 7: Asylum Knowledge
# ------------------------------------------------------------
p <- ds_asylum_applications %>%
    mutate(
        asylum_knowledge = factor(
            asylum_knowledge,
            c(
                "I have no knowledge",
                "I have basic knowledge",
                "I have a good understanding",
                "I have great expertise"
            ),
            ordered = TRUE
        )
    ) %>%
    ggplot(
        aes(
            x = as.numeric(asylum_knowledge),
            y = as.numeric(rank),
            fill = treatment,
            color = treatment,
            linetype = treatment
        )
    ) +
    facet_grid(
        country_type ~ alt,
        labeller = labeller(alt = unlist(params$alt_labels))
    ) +
    rank_style(params, k = 4, breaks = 1:4) +
    labs(
        y = "Perceived fairness\n(reversed avg. ranking)",
        x = "Knowledge of EU asylum system (1 = No knowledge, 4 = Expert)"
    )

if (interactive()) {
    print(p)
}

file.path(params$paths$figures, "asylum_knowledge.png") |>
    save_plot(width = 9, height = 5)

# ------------------------------------------------------------
#  Figure 8: Trust in the EU
# ------------------------------------------------------------

p <- ds_asylum_applications %>%
    ggplot(
        aes(
            x = trust_eu,
            y = as.numeric(rank),
            fill = treatment,
            color = treatment,
            linetype = treatment
        )
    ) +
    facet_grid(
        country_type ~ alt,
        labeller = labeller(alt = unlist(params$alt_labels))
    ) +
    rank_style(params, breaks = c(0, 5, 10)) +
    labs(
        y = "Perceived fairness\n(reversed avg. ranking)",
        x = "Trust in the EU (0 = Low, 10 = High)"
    )

if (interactive()) {
    print(p)
}

file.path(params$paths$figures, "trust_eu.png") |>
    save_plot(width = 9, height = 5)

# ------------------------------------------------------------
# ------------------------------------------------------------
# ------------------------------------------------------------
# ------------------------------------------------------------
# End!
# ------------------------------------------------------------
# ------------------------------------------------------------
# ------------------------------------------------------------
# ------------------------------------------------------------
