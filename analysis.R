# ---- Setup, include = FALSE
library(dplyr)
library(tidyr)
library(knitr)
library(ggplot2)
library(fairMigrate)
library(MASS)
library(patchwork)

opts_chunk$set(echo = FALSE)

theme_set(theme_minimal())

# ----- Data
ds <- fairMigrate::fairness_survey

# Relocation absolute 
abs_df <- data.frame(
  country = c("Germany", "Spain", "Greece", "Bulgaria", 
              "France", "Sweden", "Italy", "Poland"),
  no_relocation = c(264000, 66000, 41000, 11000, 103000, 36000, 76000, 6000),
  population = c(143000, 81000, 18000, 11000, 116000, 18000, 100000, 62000),
  gdp = c(185000, 66000, 10000, 4000, 126000, 24000, 93000, 34000)
)

rel_df <- data.frame(
  country = c("Germany", "Spain", "Greece", "Bulgaria",
              "France", "Sweden", "Italy", "Poland"),
  no_relocation = c(313, 138, 397, 178, 151, 344, 128, 16),
  population = c(169, 169, 169, 169, 169, 169, 169, 169),
  gdp = c(219, 136, 95, 65, 184, 230, 158, 92)
)

# ----- Assignment Balance
xtabs(~ relocation_treatment, ds)

# ---- % view a fair share of asylum seekers

ds <- ds %>%
  mutate(
    fair_share_lbl = case_when(
      fair_share == "No, the share is too high" ~ "1- Too high",
      fair_share == "Yes" ~ "2- Fair share",
      fair_share == "No, the share is too low" ~ "3- Too low",
      TRUE ~ fair_share
    )
  )

fair_share_question <- "Do you agree or disagree that COUNTRY is currently hosting a fair share of the total asylum seekers in the European Union? (% respondents, weighted)" # nolint

ds %>%
  count(fair_share_lbl, country, wt = weight) %>%
  mutate(pc = 100 * n / sum(n), .by = country) %>%
  pivot_wider(
    values_from = c(pc, n), names_from = fair_share_lbl
  ) %>% 
  dplyr::select(country, starts_with("pc")) %>%
  knitr::kable(caption = fair_share_question, digits = 0, 
               col.names = c("Country", "Too High", "Fair Share",
                             "Too Low", "IDK", "PNA"))

# ---- Relocation support

no_relocation_ranking_question <- "What would be a fair way to establish the number of asylum applications per country? - No Relocation (% respondents, weighted)" # nolint

ds %>%
  mutate(
     no_relocation_ranking = case_when(
      grepl("1st", no_relocation_ranking) ~ "1st",
      grepl("2nd", no_relocation_ranking) ~ "2nd",
      grepl("3rd", no_relocation_ranking) ~ "3rd",
      TRUE ~ no_relocation_ranking
     )
  ) %>% 
  count(no_relocation_ranking, country, wt = weight) %>% 
  mutate(pc = 100 * n / sum(n), .by = country) %>% 
  pivot_wider(
    values_from = c(pc, n), names_from = no_relocation_ranking
  ) %>% 
  dplyr::select(country, starts_with("pc")) %>%
  knitr::kable(caption = no_relocation_ranking_question, digits = 0,
               col.names = c("Country", "First", "Second", "Third"))



# ---- Treatment effecst

ds %>%
  mutate(
    relocation_pref = case_when(
      grepl("1st", no_relocation_ranking) ~ "No relocation",
      grepl("1st", relocation_GDP_ranking) ~ "GDP",
      grepl("1st", relocation_population_ranking) ~ "Population",
      TRUE ~ no_relocation_ranking
    )
  ) %>% 
  count(relocation_treatment, relocation_pref, 
        country, wt = weight) %>% 
  mutate(pc = 100 * n / sum(n), .by = c(relocation_treatment, country)) %>% 
  mutate(
    relocation_pref = factor(relocation_pref, c("No relocation", 
                                                "GDP", "Population"))
  ) %>%
  ggplot(aes(x = pc, y = relocation_treatment, fill = relocation_pref)) + 
  facet_wrap(~ country) +
  geom_col() +
  geom_text(aes(label = round(pc)), position = position_stack(.5)) +
  scale_fill_manual(
    name = "Top Ranked",
    values = c("GDP" = "gray", 
               "Population" = "whitesmoke", 
               "No relocation" = "orange")
  ) + 
  theme(
    axis.title = element_blank(),
  )

# ---- Regressions No relocation

fit <- polr(factor(no_relocation_ranking) ~ relocation_treatment * country, ds)

# Dfine model
model <- factor(no_relocation_ranking) ~ relocation_treatment

fit <- list()
for (j in unique(ds$country)) {
  ds_country <- dplyr::filter(ds, country == j)
  fit[[j]] <- polr(model, ds_country)
}

coef_df <- lapply(fit, broom::tidy, conf.int = TRUE) %>% 
  bind_rows(.id = "country")

p1 <- coef_df %>% 
  filter(grepl("treat", term)) %>% 
  mutate(term = gsub("relocation_treatment", "", term)) %>%
  ggplot(aes(x = reorder(country, estimate), y = estimate)) + 
  geom_linerange(aes(ymin = conf.low, ymax = conf.high)) +
  geom_point() + 
  facet_grid(~term) + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank(),
  ) + 
  geom_hline(yintercept = 0, color = "brown") +
  labs(
    y = "No relocation (prob. ranked higher)",
    caption = "Ordinal Logistic Regression coefficients",
  )

p2 <- rel_df %>%
  pivot_longer(-c(country)) %>%
  ggplot(aes(x = value / 1e3, y = name, fill = name)) +
  geom_col() +
  geom_text(aes(label = sprintf("%1.1f", value)),
            position = position_stack(.5), size = 3) +
  facet_grid(country ~ ., scales = "free") +
  theme(
    axis.title = element_blank(),
    legend.position = "bottom"
  )



# ---- Population

# Dfine model
model <- factor(relocation_population_ranking) ~ relocation_treatment

fit <- list()
for (j in unique(ds$country)) {
  ds_country <- dplyr::filter(ds, country == j)
  fit[[j]] <- polr(model, ds_country)
}

coef_df <- lapply(fit, broom::tidy, conf.int = TRUE) %>% 
  bind_rows(.id = "country")

p3 <- coef_df %>% 
  filter(grepl("treat", term)) %>% 
  mutate(term = gsub("relocation_treatment", "", term)) %>%
  ggplot(aes(x = reorder(country, estimate), y = estimate)) + 
  geom_linerange(aes(ymin = conf.low, ymax = conf.high)) +
  geom_point() + 
  facet_grid(~term) + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank(),
  ) + 
  geom_hline(yintercept = 0, color = "brown") +
  labs(
    y = "Population relocation (prob. ranked higher)",
    caption = "Ordinal Logistic Regression coefficients",
  )


# ---- plot-relocation-rankings, fig.width = 9, fig.height = 9
(p1 + p3) / p2 & plot_annotation(tag_levels = "A")


# ---- ctrl-attitudes

model <- factor(no_relocation_ranking) ~ relocation_treatment + fair_share_lbl

fit <- list()
for (j in unique(ds$country)) {
  ds_country <- dplyr::filter(ds, country == j)
  fit[[j]] <- polr(model, ds_country)
}

coef_df <- lapply(fit, broom::tidy, conf.int = TRUE) %>% 
  bind_rows(.id = "country")

p4 <- coef_df %>% 
  filter(grepl("treat", term)) %>% 
  mutate(term = gsub("relocation_treatment", "", term)) %>%
  ggplot(aes(x = reorder(country, estimate), y = estimate)) + 
  geom_linerange(aes(ymin = conf.low, ymax = conf.high)) +
  geom_point() + 
  facet_grid(~term) + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank(),
  ) + 
  geom_hline(yintercept = 0, color = "brown") +
  labs(
    y = "Population relocation (prob. ranked higher)",
    caption = "Ordinal Logistic Regression coefficients",
  )

p4 


# ---- by-attitudes

model <- factor(no_relocation_ranking) ~ relocation_treatment
attitude <- c("1- Too high", "2- Fair share", "3- Too low")

fit <- list()
for (j in unique(ds$country)) {
  for (k in attitude) {
    ds_filtered <- dplyr::filter(ds, country == j, fair_share_lbl == k)
    fit[[paste(j, k)]] <- polr(model, ds_filtered)
  }
}

coef_df <- lapply(fit, broom::tidy, conf.int = TRUE) %>% 
  bind_rows(.id = "country_attitude")

p5 <- coef_df %>% 
  filter(grepl("treat", term)) %>% 
  mutate(term = gsub("relocation_treatment", "", term)) %>%
  mutate(attitude = gsub(".*([0-9]+.*)", "\\1", country_attitude)) %>%
  mutate(country = gsub("(.*)([0-9]+.*)", "\\1", country_attitude)) %>%
  ggplot(aes(x = reorder(country, estimate), y = estimate, color = term)) + 
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), 
                 position = position_dodge(.5)) +
  geom_point(position = position_dodge(.5)) + 
  facet_grid(~term) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank(),
  ) + 
  geom_hline(yintercept = 0, color = "brown") +
  labs(
    y = "No relocation (prob. ranked higher)",
    caption = "Ordinal Logistic Regression coefficients",
  ) + 
  facet_grid(~ attitude)

p5
