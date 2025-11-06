# ----- setup, include = FALSE
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(yaml)
library(knitr)
library(kableExtra)
library(fairMigrate)
library(here)

opts_chunk$set(error = TRUE, echo = FALSE)

source("R/theme.R")

# ---- functions, include = FALSE -----

pretty_table <- function(...) {
  istex <- knitr::is_latex_output()
  kableExtra::kbl(..., booktabs = istex) %>%
    kableExtra::kable_styling() %>%
    kableExtra::column_spec(1, bold = TRUE)
}

categorise_trust <- function(x) {
  as.numeric(gsub("[^0-9]+", "", x)) %>%
  cut(breaks = c(0, 3, 6, 10), include.lowest = TRUE, 
      labels = c("Low", "Neutral", "High"))
}

# ----- data, include = FALSE -----

ds <- fairMigrate::fairness_survey %>% 
  mutate(
    parents_abroad = case_when(
      country_mother != country ~ "Abroad",
      country_father != country ~ "Abroad",
      TRUE ~ "Local"
    ), 

    trust_cut = trust %>% categorise_trust(),
    trust_eu_cut = trust_eu %>% categorise_trust(),

    political_right_num = as.numeric(gsub("[^0-9]+", "", political_right)),
    political_right_cut = cut(
      political_right_num, 
      c(0, 3, 6, 10), 
      include.lowest = TRUE,
      labels = c("left", "neutral", "right")
    ) %>% factor(c("neutral", "left", "right"))
  )

cols <- grep("^(no|rel).*_ranking", names(ds), value = TRUE)

ds_long <- ds %>%
  mutate(respondent_id = row_number()) %>%
  pivot_longer(
    all_of(cols),
    names_to = "alt",
    values_to = "choice"
  ) %>% 
  mutate(
    relocation_treatment = case_when(
      relocation_treatment == "Control" ~ "No info",
      relocation_treatment == "Absolute" ~ "Info: Absolute",
      relocation_treatment == "Relative" ~ "Info: Relative",
    ),
    relocation = case_when(
      grepl("no_relocation", alt) ~ "No relocation",
      grepl("GDP", alt) ~ "Relocation: by GDP",
      grepl("population", alt) ~ "Relocation: by pop",
    ),
    rank = as.numeric(gsub("[^0-9]+", "", choice))
  )


ds_asylum_rel <- data.frame(
  country = c("Germany", "Spain", "Greece", "Bulgaria",
              "France", "Sweden", "Italy", "Poland"),
  no_relocation = c(313, 138, 397, 178, 151, 344, 128, 16),
  population = c(169, 169, 169, 169, 169, 169, 169, 169),
  gdp = c(219, 136, 95, 65, 184, 230, 158, 92)
)

#' # Appendix

#' ## AS relocation 
#' 
#' 

tbl_relocation <- ds_asylum_rel %>% 
    mutate(
        diff_no_relocation_gdp = no_relocation - gdp,
        diff_no_relocation_pop = no_relocation - population
    ) %>% 
    arrange(country) %>%
    select(
        "Country" = country,
        "No Relocation (Baseline)" = no_relocation,
        "Relocation by GDP Share" = gdp,
        "Relocation by Population Share" = population,
        "Difference: Baseline - by GDP" = diff_no_relocation_gdp,
        "Difference: Baseline - by Pop." = diff_no_relocation_pop
    )
 



#' ## Fairest Relocation Schemes
#' 
#' 


# ----- fairest relocation, fig.asp = .5, fig.cap = cap ----

cap <- paste(
  "Perceptions of the Fairest Relocation Scheme."
)

ds_top_rank <- ds_long %>% 
  bind_rows(mutate(., country = "Total")) %>%
  count(relocation_treatment, relocation, rank, country) %>%
  mutate(percent = 100 * n / sum(n),
         .by = c(country, rank, relocation_treatment)) %>%
  filter(rank == 1, relocation_treatment == "No info")

obs_no_info <- sum(ds_top_rank$n) / 2 # remove total

plot_top_rank <- ds_top_rank %>%
  mutate(
    mean_group = percent[relocation == relocation[1]],
    .by = country
  ) %>%
  ggplot(
    aes(
      x = percent,
      y = reorder(country, mean_group),
      fill = relocation,
      label = sprintf("%2.0f%%", percent),
      color = grepl("by pop", relocation)
    )
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_color_manual(values = c("black", "white")) +
  scale_fill_brewer(palette = "Blues") +
  facet_grid(country == "Total" ~ relocation, 
              scales = "free", space = "free") +
  geom_col(color = NA) +
  geom_text(position = position_stack(.5))

plot_top_rank +
  labs(
    title = "Perceptions of the Fairest Relocation Scheme",
    subtitle = "% respondents per country who chose a given option as their top choice.",
    caption = sprintf(
      "Note: %s (N = %i)",
      "Subsample of participants in the 'No Information' treatment group.",
      obs_no_info 
    ) 
  )


# ----- mean rank, fig.asp = .5, eval = FALSE ----- 

ds_mean_rank <- ds_long %>%
  reframe({
    fit <- lm(rank ~ relocation_treatment + sex + age_cat + ISCED)
    broom::tidy(fit, conf.int = TRUE)
  }, .by = c(country, relocation)) %>%
  mutate(
    term = gsub("relocation_treatment", "", term)
  )

g <- ds_mean_rank %>%
  filter(grepl("treatment", term)) %>%
  ggplot(
    aes(
      x = estimate,
      xmin = conf.low,
      xmax = conf.high,
      y = country,
      color = relocation,
      alpha = abs(estimate) > 1.96 * std.error,
    )
  ) +
  scale_alpha_manual(values = c(0.2, 1)) +
  scale_x_continuous(limits = c(-1, 1)) +
  geom_vline(xintercept = 0) +
  facet_grid(term ~ relocation) +
  geom_linerange() +
  geom_point()  



# ----- combined plot, fig.asp = 1, eval = FALSE -----

theme_update(
  legend.position = "none",
  strip.text.y = element_blank(),
  axis.text.x = element_text(),
)

upper <- g %+% filter(ds_mean_rank, grepl("No info", term)) 
lower <- g %+% filter(ds_mean_rank, grepl("Relative", term))
combined <- (upper) / (lower)

combined +
  plot_annotation(
    tag_levels = "A",
    title = "Treatment Effect on Perceived Fairness of Relocation Alternatives",
    subtitle = "Absolute vs No information (A); Absolute vs Relative Information (B)",
    caption = "Note. Mean rank differences using OLS, with age, sex, & educ. controls. A negative difference indicates more fairness." # nolint
  )


#' ## Mean Rank Differences 
#' 
#' 


ds_main <- ds_long %>% 
  reframe({
    fit <- lm(rank ~ relocation_treatment)
    broom::tidy(fit, conf.int = TRUE)
  }, .by = c(country, relocation))


# ---- mean-rank-diff, fig.asp = 1, fig.cap = cap --- 

cap <- paste(
  "Treatment effects: Differences in mean ranks between the Absolute Info group and all other groups for each relocation scheme."
)

data_plot <- ds_main %>% 
  filter(grepl("relocation_treatment", term)) %>%
  left_join(ds_asylum_rel, by = "country") %>%
  mutate(
    country_color = ifelse(no_relocation > population, "#050557", "#8a7605"),
    country = sprintf("<span style='color:%s'>%s</span>", country_color, country),
    term = gsub("relocation_treatment", "", term) %>%
      gsub(":sex", " X ", .) %>%
      paste("Info: Abs vs", .)
  ) 

theme_update(
  axis.text.x = element_text(),
  legend.position = "right", 
  legend.title = element_blank()
)

g <- data_plot %>%
  ggplot(
    aes(
      x = estimate,
      xmin = conf.low,
      xmax = conf.high,
      y = reorder(country, no_relocation - population, FUN=\(x) mean(x, na.rm = TRUE)),
      color = ifelse(
        abs(estimate) > 1.96 * std.error,
        "p < 0.05",
        "p > 0.05"
      )
    )
  ) + 
  facet_wrap(. ~ term) +
  scale_color_brewer(palette = "Paired", direction = -1) +
  geom_hline(yintercept = 4.5, linetype = 2, linewidth = 0.2) +
  geom_vline(xintercept = 0) +
  geom_linerange() +
  geom_point() + 
  theme(
    panel.background = element_rect(color = "gray"),
  )


g <- g + 
  scale_x_continuous(
    breaks = c(-1, 1) * 0.4, 
    labels = c("More fair", "Less fair"), 
    limits = c(-1, 1) * 0.7
  ) 

# g + 
#   facet_wrap(relocation ~ term, ncol = 2) +
#   labs(
#     title = "Perceived Fairness by information condition and country",
#     subtitle = "Mean Rank difference between Absolute Info and other groups",
#   )


#' Combine plots using patchwork
#' 
#' 

# ----- combined-mean-diff, fig.asp = 1, fig.cap = cap -----

cap <- paste(
  "Mean differences in average fairness rank between Absolute information and other information group.",
  "under each relocation scheme: A. no relocation; B. relocation by population; C. relocation by GDP.",
  "Countries ordered by difference between asylum seekers under relocation by population and no relocation."
)

upper <- g %+% filter(data_plot, grepl("No relocation", relocation)) 
middle <- g %+% filter(data_plot, grepl("Relocation: by pop", relocation))
lower <- g %+% filter(data_plot, grepl("Relocation: by GDP", relocation))
combined <- (upper) / (middle) / (lower)

combined + 
  plot_layout(guides = "collect") & 
  plot_annotation(tag_levels = "A")


#' 
#' 
#' ---------------------------------------------------------------
#' 
#' 

theme_update(
  legend.position = "none"
)


#' ## Sex
#' 
#' 

xtabs( ~ country + sex, ds, exclude = c("Other", "Refusal")) %>% 
  addmargins(margin = 1, FUN = list(Total = sum)) %>%
  pretty_table(caption = "Sex by country", align = "lrr")

ds_sex_interact <- ds_long %>% 
  filter(sex %in% c("Female", "Male")) %>%
  reframe({
    fit <- lm(rank ~ relocation_treatment * sex)
    broom::tidy(fit, conf.int = TRUE)
  }, .by = c(country, relocation))


# ---- sex interactions, fig.cap = cap ---- 

cap <- paste(
  "Mean fairness rank differences by sex."
)

ds_sex_interact %>% 
  filter(grepl("sex", term)) %>%
  mutate(
    term = gsub("relocation_treatment", "", term) %>%
      gsub(":sex", " X ", .)
  ) %>%
  ggplot(
    aes(
      x = estimate,
      xmin = conf.low,
      xmax = conf.high,
      y = country,
      color = abs(estimate) > 1.96 * std.error
    )
  ) + 
  scale_color_brewer(palette = "Paired") +
  facet_wrap(relocation ~ term) + 
  geom_vline(xintercept = 0) +
  geom_linerange() +
  geom_point()


#' ## Education
#' 
#' 

xtabs( ~ country + ISCED2, ds, exclude = "Unknown") %>% 
  addmargins(margin = 1, FUN = list(Total = sum)) %>%
  pretty_table(caption = "Highest education by country", align = "lrr")

# ---- education interactions, fig.cap = cap ---- 

cap <- paste(
  "Mean fairness rank differences by high and low/mid educational achievement."
)

ds_educ <- ds_long %>% 
  filter(!ISCED2 %in% c("Unknown")) %>%
  reframe({
    fit <- lm(rank ~ relocation_treatment * ISCED2)
    broom::tidy(fit, conf.int = TRUE)
  }, .by = c(country, relocation))

ds_educ %>% 
  filter(grepl(":ISCED", term)) %>%
  mutate(
    term = gsub("relocation_treatment", "", term) %>%
      gsub(":ISCED2", " X ", .)
  ) %>%
  ggplot(
    aes(
      x = estimate,
      xmin = conf.low,
      xmax = conf.high,
      y = country,
      color = abs(estimate) > 1.96 * std.error
    )
  ) + 
  scale_color_brewer(palette = "Paired") +
  facet_wrap(relocation ~ term) + 
  geom_vline(xintercept = 0) +
  geom_linerange() +
  geom_point()


# ---- parents abroad interactions ---- 

#' ## Parents born abroad
#' 
#' 


xtabs( ~ country + parents_abroad, ds) %>% 
  addmargins(margin = 1, FUN = list(Total = sum)) %>%
  pretty_table(caption = "Parents born abroad vs local")


ds_interact <- ds_long %>% 
  reframe({
    fit <- lm(rank ~ relocation_treatment * parents_abroad)
    broom::tidy(fit, conf.int = TRUE)
  }, .by = c(country, relocation))

ds_interact %>% 
  filter(grepl(":parents_abroad", term)) %>%
  mutate(
    term = gsub("relocation_treatment", "", term) %>%
      gsub(":parents_abroad", " X ", .)
  ) %>%
  ggplot(
    aes(
      x = estimate,
      xmin = conf.low,
      xmax = conf.high,
      y = country,
      color = abs(estimate) > 1.96 * std.error
    )
  ) + 
  scale_color_brewer(palette = "Paired") +
  facet_wrap(relocation ~ term) + 
  geom_vline(xintercept = 0) +
  geom_linerange() +
  geom_point()


#' ## Political right
#' 
#' 



# ----- political-right ---- 

#grep("right", names(ds), value = TRUE)

xtabs( ~ country + political_right_cut, ds) %>% 
  addmargins(margin = 1, FUN = list(Total = sum)) %>%
  pretty_table(caption = "Political right by country")


ds_interact <- ds_long %>% 
  reframe({
    fit <- lm(rank ~ relocation_treatment * political_right_cut)
    broom::tidy(fit, conf.int = TRUE)
  }, .by = c(country, relocation))

ds_interact %>% 
  filter(grepl(":political_right_cut", term)) %>%
  mutate(
    term = gsub("relocation_treatment", "", term) %>%
      gsub(":political_right_cut", " X ", .)
  ) %>%
  ggplot(
    aes(
      x = estimate,
      xmin = conf.low,
      xmax = conf.high,
      y = country,
      color = abs(estimate) > 1.96 * std.error
    )
  ) + 
  scale_y_discrete(
    labels = c("Greece", "Sweden", "Germany", "Bulgaria", 
               "France", "Spain", "Italy", "Poland")
  ) +
  scale_color_brewer(palette = "Paired") +
  facet_wrap(relocation ~ term) + 
  geom_vline(xintercept = 0) +
  geom_linerange() +
  geom_point()




# ---- trust interactions ---- 

#' ## Trust

xtabs( ~ country + trust_cut, ds) %>% 
  addmargins(margin = 1, FUN = list(Total = sum)) %>%
  pretty_table(caption = "Trust in Others By country")


ds_interact <- ds_long %>% 
  reframe({
    fit <- lm(rank ~ relocation_treatment * trust_cut)
    broom::tidy(fit, conf.int = TRUE)
  }, .by = c(country, relocation))

ds_interact %>% 
  filter(grepl(":trust_cut", term)) %>%
  mutate(
    term = gsub("relocation_treatment", "", term) %>%
      gsub(":trust_cut", " X ", .)
  ) %>%
  ggplot(
    aes(
      x = estimate,
      xmin = conf.low,
      xmax = conf.high,
      y = country,
      color = abs(estimate) > 1.96 * std.error
    )
  ) + 
  scale_color_brewer(palette = "Paired") +
  facet_wrap(relocation ~ term) + 
  geom_vline(xintercept = 0) +
  geom_linerange() +
  geom_point()

# ---- trust EU interactions ---- 

#' ## Trust EU

xtabs( ~ country + trust_eu_cut, ds) %>% 
  addmargins(margin = 1, FUN = list(Total = sum)) %>%
  pretty_table(caption = "Trust in Others By country")


ds_interact <- ds_long %>% 
  reframe({
    fit <- lm(rank ~ relocation_treatment * trust_eu_cut)
    broom::tidy(fit, conf.int = TRUE)
  }, .by = c(country, relocation))

ds_interact %>% 
  filter(grepl(":trust_eu_cut", term)) %>%
  mutate(
    term = gsub("relocation_treatment", "", term) %>%
      gsub(":trust_eu_cut", " X ", .)
  ) %>%
  ggplot(
    aes(
      x = estimate,
      xmin = conf.low,
      xmax = conf.high,
      y = country,
      color = abs(estimate) > 1.96 * std.error
    )
  ) + 
  scale_color_brewer(palette = "Paired") +
  facet_wrap(relocation ~ term) + 
  geom_vline(xintercept = 0) +
  geom_linerange() +
  geom_point()

