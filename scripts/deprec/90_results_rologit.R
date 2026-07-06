suppressMessages({
    library(dplyr)
    library(tidyr)
    library(ggplot2)
    library(ggtext)
    library(patchwork)
    library(yaml)
    library(knitr)
    library(kableExtra)
    library(mlogit)
})

source("R/theme.R")

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

ds_asylum_rel <- data.frame(
    country = c("Germany", "Spain", "Greece", "Bulgaria",
                "France", "Sweden", "Italy", "Poland"),
    no_relocation = c(313, 138, 397, 178, 151, 344, 128, 16),
    population = c(169, 169, 169, 169, 169, 169, 169, 169),
    gdp = c(219, 136, 95, 65, 184, 230, 158, 92)
)

# ----------------------------------------------------
# Process data
# ----------------------------------------------------

ds <- ds_survey %>%
    dplyr::mutate(
        across(
            starts_with("trust"), 
            ~ cut(., c(0, 3, 6, 10), labels = c("low", "middle", "high"))
        ),
        across(
            starts_with("political_right"), 
            ~ cut(., c(0, 3, 6, 10), labels = c("left", "neutral", "right"))
        )
    )

ds_asylum_rel_long <- ds_asylum_rel %>%
  pivot_longer(c(no_relocation, population, gdp), 
               names_to = "alt",
               values_to = "asyl_skrs_rel") %>%
  mutate(
    alt = case_when(
      alt == "gdp" ~ "relocation_GDP_ranking",
      alt == "population" ~ "relocation_population_ranking",
      alt == "no_relocation" ~ "no_relocation_ranking"
    ) %>% as.factor()
  )

ds_combined <- ds_long %>%
    left_join(ds_asylum_rel_long, by = c("country", "alt")) %>% 
    dplyr::mutate(
        info = gsub("Info: ?", "", relocation_treatment)
    )

# ----------------------------------------------------
# Multinomial logit preparation
# ----------------------------------------------------

ds_mlogit <- ds_combined %>%
    dplyr::select(
        respondent_id, alt, choice = rank, edu = ISCED, 
        info, country, asyl_skrs_rel
    ) %>% 
    mutate(across(where(is.character), as.factor)) %>%
    dfidx::dfidx(
        idx = c("respondent_id", "alt"),
        choice = "choice",
        ranked = TRUE
    )

# ----------------------------------------------------
# Fit the baseline model 
# ----------------------------------------------------

model_spec <- choice ~ 0 | info + country + edu
fit <- mlogit(model_spec, data = ds_mlogit)

coeffs <- broom::tidy(fit, conf.int = TRUE, exponentiated = TRUE)

knitr::kable(coeffs, digits = 2)

plot_base <- coeffs %>% 
    ggplot(aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high)) +
    geom_vline(xintercept = 0, color = "red") +
    geom_errorbar(width = 0.2) +
    geom_point() + 
    scale_y_discrete(
        labels = function(x) {
            gsub("country(.*):relocation_(.*)_ranking", "\\1 - \\2", x)
        }
    )

plot_base


# ----------------------------------------------------
# Multinomial logit by information condition
# ----------------------------------------------------

treatments <- unique(ds_long$relocation_treatment)

model_spec <- choice ~ scale(asyl_skrs_rel)

coeffs <- split(ds_mlogit, ds_mlogit$info) %>% 
    lapply(function(d){
        fit <- mlogit(model_spec, data = d)
        broom::tidy(fit, conf.int = TRUE)
    }) %>% 
    bind_rows(.id = "treatment")

theme_set(theme_classic())
theme_update(
    legend.position = "none",
    #panel.background = element_rect(color="black"),
    panel.grid.major.x = element_line(),
)

plot_base %+% coeffs + 
    facet_grid(term ~ .) + 
    aes(color = treatment, y = treatment) +
    scale_x_continuous(limits = c(-1, 1)) +
    scale_color_brewer(palette = "Set1") + 
    labs(
        x = "Estimate (95% CIs)", y = NULL
    )


# ----------------------------------------------------
# Multinomial logit by COUNTRY
# ----------------------------------------------------

model_spec <- choice ~ 0 | info

ds_mlogit <- ds_mlogit %>% 
    mutate(info = relevel(factor(info), ref = "No info"))

coeffs <- split(ds_mlogit, ds_mlogit$country) %>% 
    lapply(function(d){
        fit <- mlogit(model_spec, data = d)
        broom::tidy(fit, conf.int = TRUE)
    }) %>% 
    bind_rows(.id = "country")

plot_base %+% coeffs + 
    facet_wrap(country ~ .) + 
    aes(y = term) +
    #scale_x_continuous(limits = c(-1, 1)) +
    #scale_color_brewer(palette = "Set1") + 
    labs(
        x = "Estimate (95% CIs)", y = NULL
    ) + 
    scale_y_discrete(
        labels = function(x) {
            gsub("relocation_(.*)_ranking", "by \\1", x) %>%
            gsub(":", " x ", .)
        }
    )

# ----------------------------------------------------
# OLD
# ----------------------------------------------------



ds_mlogit <- ds_combined %>%
  dplyr::select(
    respondent_id, alt, rank, 
    relocation_treatment, 
    age_cat, sex, ISCED,
    asyl_skrs_rel,
    political_right_cut,
    trust_eu_cut
  ) %>%
  mlogit.data(
    chid.var = "respondent_id",
    alt.var = "alt",
    shape = "long",
    ranked = TRUE,
    choice = "rank"
  )

# ----------------------------------------------------
# Baseline model
# ----------------------------------------------------

model <- rank ~ scale(asyl_skrs_rel) | relocation_treatment | 0
fit <- mlogit(model, ds_mlogit)

coeffs <- broom::tidy(fit, conf.int = TRUE) 

coeffs %>% 
    mutate(
        term = gsub("relocation_treatment", "", term) %>%
            gsub("relocation_(.*)_ranking", "Alt (\\1)", .) %>% 
            gsub("scale.*", "Asylum seekers (standard)", .),
        label = case_match(
            term,
            .default = term,
            "(Intercept):Alt (GDP)" ~ "Intercept x relocation by GDP",
            "(Intercept):Alt (population)" ~ "Intercept x relocation by population",
            "Asylum seekers (standard)" ~ "Asylum seekers (standardised)",
            "Control:Alt (GDP)" ~ "No info (control) x relocation by GDP",
            "Control:Alt (population)" ~ "No info (control) x relocation by population",
            "Relative:Alt (GDP)" ~ "AS per capita (relative) x relocation by GDP",
            "Relative:Alt (population)" ~ "AS per capita (relative) x relocation by population",
        )
    ) %>% 
    mutate(
        cis = sprintf("[%.2f, %.2f]", conf.low, conf.high),
        p.value = case_when(
            p.value < 0.05 ~ "<0.05",
            TRUE ~ sprintf("%2.2f", p.value),
        )
    ) %>%
    select(
        Term = label, 
        Estimate = estimate, 
        SE = std.error, 
        P.value = p.value, 
        "95% CI" = cis,
    ) %>% 
    pretty_table(
        digits = 2,
        caption = "Baseline rank-ordered logit regression model on fairness rankings"
    )


# ---- by-treatment ----

treatments <- unique(ds_long$relocation_treatment)
model <- rank ~ 0 + scale(asyl_skrs_rel) | 1

coeff_list <- list()
for (j in treatments) {
  fit <- mlogit(model, ds_mlogit, subset = relocation_treatment == j)
  coeff_list [[j]] <- broom::tidy(fit, conf.int = TRUE, exponentiated = TRUE)
}

coeffs <- bind_rows(coeff_list, .id = "treat") %>%
    mutate(
        term = gsub("relocation_treatment", "", term) %>%
            gsub("relocation_(.*)_ranking", "Alt (\\1)", .) %>% 
            gsub("scale.*", "Asylum seekers (standard)", .),
        label = case_match(
            term,
            .default = term,
            "(Intercept):Alt (GDP)" ~ "Relocation by GDP (vs. no relocation)",
            "(Intercept):Alt (population)" ~ "Relocation by population (vs. no relocation)",
            "Asylum seekers (standard)" ~ "Asylum seekers (1 SD change)",
            "Control:Alt (GDP)" ~ "No info (control) x relocation by GDP",
            "Control:Alt (population)" ~ "No info (control) x relocation by population",
            "Relative:Alt (GDP)" ~ "AS per capita (relative) x relocation by GDP",
            "Relative:Alt (population)" ~ "AS per capita (relative) x relocation by population",
        ),
        cis = sprintf("[%.2f, %.2f]", conf.low, conf.high),
        p.value = case_when(
            p.value < 0.05 ~ "<0.05",
            TRUE ~ sprintf("%2.2f", p.value),
        )
    ) %>%
    mutate(
        group_first_row = ifelse(row_number() == 1, treat, ""),
        .by = treat
    )


coeffs %>% 
    arrange(term) %>%
    mutate(lable_first_row = ifelse(row_number()==1, label, ""), .by = label) %>% 
    select(
        Term = lable_first_row, 
        Treatment = treat,
        Estimate = estimate, 
        SE = std.error, 
        P.value = p.value, 
        "95% CI" = cis,
    ) %>% 
    pretty_table(
        digits = 2,
        caption = "Rank-ordered logit regression on fairness rankings by treatment groups"
    )

# ---- plot-by-treatment, fig.cap = cap, fig.width = 5, fig.asp = .75 --- 

cap <- paste(
  "Treatment effects"
)

g <- coeffs %>% 
  ggplot(
    aes(
      x = estimate,
      xmin = conf.low,
      xmax = conf.high,
      y = treat,
      color = treat
    )
  ) +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(~ label, ncol = 1, strip.position = "top") +
  geom_vline(xintercept = 0) +
  geom_linerange() + 
  geom_point() + 
  theme(
    panel.background = element_rect(color = "gray"),
    strip.text.y = element_text(angle = 0),
    axis.text.x = element_text(),
  )
 
g

# ---- plot-v2, fig.cap = cap, fig.width = 6, fig.asp = 1 --- 

g2 <- g +
scale_x_continuous(
    name = "Log-odds estimate (95% CI)",
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  scale_y_discrete(
    name = ""
  ) +
  guides(
    color = guide_legend(title = "Treatment group", override.aes = list(size = 3)),
    shape = guide_legend(title = "Treatment group")
  ) +
  theme_minimal(base_size = 12, base_family = "Helvetica") +
  theme(
    panel.background = element_rect(color = 'gray85'),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray85", linewidth = 0.3),
    strip.background = element_rect(fill = "gray95", color = NA),
    strip.text = element_text(face = "bold", size = 11, hjust = 0),
    axis.text = element_text(color = "black", size = 10),
    axis.title.x = element_text(size = 11, margin = margin(t = 6)),
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    plot.caption = element_text(size = 9, hjust = 0, color = "gray40", margin = margin(t = 10)),
    plot.margin = margin(1, 1, 1, 1, unit = "lines"),
    #panel.spacing = unit(1, "lines")
  ) + 
  annotate("text", x = Inf, y = 0.5, label = "More fair", hjust = 1.5, vjust = 0, size = 2.5, color = "gray20") +
  annotate("text", x = -Inf, y = 0.5, label = "Less fair", hjust = -.5, vjust = 0, size = 2.5, color = "gray20")

g2 
