# ---- setup, include = FALSE
require(dplyr)
require(tidyr)
require(knitr)

knitr::opts_chunk$set(error = TRUE)

data_path <- file.path("data", "processed", "fairness_survey_clean.rds")

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


