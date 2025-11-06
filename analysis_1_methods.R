# ---- setup, include = FALSE
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

knitr::opts_chunk$set(error = TRUE)

data_path <- file.path("data", "processed", "fairness_survey_clean.rds")

# ---- illustration ------------------------

tbl_example <- data.frame(
  Category = c(
    "Average yearly asylum applications (2014-23)",
    "Most recent population",
    "Most recent GDP",
    "No relocation",
    "Relocation by population",
    "Relocation by GDP"
  ),
  country_a = c(
    15000,
    15e6,
    1000e6,
    15000,
    30000,
    25000
  ),
  country_b = c(
    35000,
    10e6,
    1000e6,
    35000,
    20000,
    25000
  ),
  check.names = FALSE
)

format_numbers <- function(x) {
    dplyr::case_when(
        x >= 1e9 ~ sprintf("%.1fB", x / 1e9),
        x >= 1e6 ~ sprintf("%.1fM", x / 1e6),
        x >= 1e3 ~ sprintf("%.1fK", x / 1e3),
        TRUE ~ as.character(round(x, 1))
    )
}

tbl_example <- tbl_example %>% 
    mutate(
        total = country_a + country_b,
        per_capita_a = country_a / (country_a[Category == "Most recent population"] / 1e5),
        per_capita_b = country_b / (country_b[Category == "Most recent population"] / 1e5),
        across(where(is.numeric), format_numbers),
    )

kbl_example <- tbl_example %>% 
    dplyr::select(
        Category = Category,
        "Country A (absolute)" = country_a,
        "Country A (per 100k)" = per_capita_a,
        "Country B (absolute)" = country_b,
        "Country B (per 100k)" = per_capita_b,
        "Total" = total
    ) %>%
    kbl(
        caption = "Illustrative example of asylum applications for two hypothetical countries",
        align = c("l", "r", "r", "r", "r", "r"),
        booktabs = TRUE,
        linesep = "",
    ) %>%
    kable_styling(full_width = TRUE) %>%
    row_spec(0, bold = TRUE) %>%
    column_spec(1, width = "5cm") %>%
    footnote(
        general = "Values formatted for readability. Per capita = per 100,000 population.",
        general_title = "",
        footnote_as_chunk = TRUE
    )

# ---- eurostat ------------------------ 

tbl_asylum_rel <- data.frame(
  country = c("Germany", "Spain", "Greece", "Bulgaria",
              "France", "Sweden", "Italy", "Poland"),
  no_relocation = c(313, 138, 397, 178, 151, 344, 128, 16),
  population = c(169, 169, 169, 169, 169, 169, 169, 169),
  gdp = c(219, 136, 95, 65, 184, 230, 158, 92)
)

kbl_asylum_rel <- tbl_asylum_rel %>% 
    dplyr::select(
      Country = country,
      "No relocation (per 100k)" = no_relocation,
      "By population (per 100k)" = population,
      "By GDP (per 100k)" = gdp
    ) %>%
    kbl(
        caption = "Allocation rules per capita",
        booktabs = TRUE,
        linesep = "",
    ) %>% 
    kable_styling(full_width = TRUE)


# ---- data ------------------------
ds <- readRDS(data_path)

ds$male <- 100 * (ds$sex == "Male")
ds$educ_high <- 100 * (ds$ISCED == "High")
ds$educ_mid <- 100 * (ds$ISCED == "Mid")
ds$educ_low <- 100 * (ds$ISCED == "Low")

ds$asylum_knowledge <- as.numeric(factor(
    ds$asylum_knowledge,
    levels = c(
        "I have no knowledge",
        "I have basic knowledge",
        "I have a good understanding",
        "I have great expertise"
    )
))

ds$fair_share <- as.numeric(factor(
    ds$fair_share,
    levels = c(
        "No, the share is too low",
        "Yes",
        "No, the share is too high"
    )
))

vars <- c("burden_sharing_important", 
          "equal_rights_important", 
          "trust", "trust_gov", "trust_eu",
          "equal_distribution_important",
          "equal_treatment_important",
          "asylum_distr_even",
          "quick_processing_important")


ds <- mutate(
    ds,
    across(all_of(vars), \(x) as.numeric(gsub("[^0-9]+", "", x)))
)

# ---- random-assignment ------------------------

tbl_assignment <- xtabs(~ country + relocation_treatment, ds) %>% 
    addmargins(c(1,2), FUN = list(Total = sum), quiet = TRUE)    

kbl_assignment <- tbl_assignment %>% 
    kableExtra::kbl(
        caption = "Random assignment by country",
        format.args = list(big.mark = " "),
        booktabs = TRUE,
        linesep = c(rep("", 7), "\\addlinespace")
    ) %>% 
    kableExtra::kable_styling(full_width = TRUE) %>%
    column_spec(1, bold = TRUE)


# ---- covariate-balance ------------------------

covariates <- c(
    "age", "male",
    "educ_high", "educ_mid", "educ_low",
    "trust", "trust_eu", "trust_gov", 
    "asylum_knowledge", 
    "fair_share",
    "asylum_distr_even", 
    "burden_sharing_important",
    "equal_rights_important",
    "equal_treatment_important",
    "equal_distribution_important"
)

sd_pooled <- ds %>% 
    summarise(across(all_of(covariates), \(x) sd(x, na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "sd_pooled")

table_balance <- lapply(covariates, \(v) {
    form <- reformulate("relocation_treatment + country", v)
    fit <- lm(
        form,
        mutate(ds, relocation_treatment = relevel(factor(relocation_treatment), ref = "Control")), 
        contrasts = list(country = "contr.sum"), 
        weights = weight
    )
    
    results <- broom::tidy(fit) %>%
        filter(grepl("treat|Intercept", term)) %>% 
        mutate(variable = v)

    balance <- left_join(results, sd_pooled, by = "variable") %>%
        mutate(std_diff = estimate / sd_pooled)

    balance
}) %>% 
    bind_rows() %>%
    mutate(
        stat = ifelse(
            term == "(Intercept)",
            sprintf("%.1f (%.1f)", estimate, sd_pooled),
            sprintf("%.2f [%.2f]", estimate, std_diff)
        )
    ) %>% 
    select(term, variable, stat) %>%
    mutate(term = gsub("relocation_treatment", "", term)) %>%
    pivot_wider(
        names_from = term,
        values_from = stat
    ) %>%
    mutate(
        variable = case_match(
            variable,
            "age" ~ "Age (years)",
            "male" ~ "Male (%)",
            "educ_high" ~ "High education (%)",
            "educ_mid" ~ "Mid education (%)",
            "educ_low" ~ "Low education (%)",
            "trust" ~ "Trust others (0-10)",
            "trust_eu" ~ "Trust in the EU (0-10)",
            "trust_gov" ~ "Trust in government (0-10)",
            "asylum_knowledge" ~ "Knowledge of asylum System (1 = no, 4 = high)",
            "asylum_distr_even" ~ "Perceived evenness of AS distribution in EU (0-10)",
            "fair_share" ~ "Perceived AS in own country (1 = too few, 3 = too many)",
            "burden_sharing_important" ~ "Importance of burden sharing (0-10)",
            "equal_distribution_important" ~ "Importance of equal distribution (0-10)",
            "equal_treatment_important" ~ "Importance of equal treatment (0-10)",
            "equal_rights_important" ~ "Importance of equal rights (0-10)",
            .default = variable
        )
    ) %>%
    select(
        Variable = variable,
        Control = "(Intercept)",
        "Diff. Absolute" = Absolute,
        "Diff. Relative" = Relative
    )


kbl_balance <- table_balance %>% 
    kable(
        booktabs = TRUE,
        digits = 1,
        caption = "Means and standard deviations for the Control group, and mean differences between the Control group and other groups (standardized differences in square brackets). AS = asylum seekers."
    )
