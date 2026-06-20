# ----------------------------------------------------
# Descriptive analysis
# Author: Andrea Blasco
# ----------------------------------------------------
suppressMessages({
    library(dplyr)
    library(kableExtra)
    library(flextable)
})

results_dir <- file.path("results", c("figures", "tables", "models"))
for (dir in results_dir) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
}
tab_dir <- file.path("results", "tables")
data_dir <- file.path("data", "processed")

ds_survey_filename <- file.path(data_dir, "fair_survey_clean.rds")
ds_long_filename <- file.path(data_dir, "fair_survey_long.rds")

all_tables_filename <- file.path("results", "tables", "descriptives.rds")

# ----------------------------------------------------
# Process data
# ----------------------------------------------------

ds <- readRDS(ds_survey_filename)
message("Loaded ", ds_survey_filename)

ds <- ds %>%
    dplyr::mutate(
        relocation_treatment = relevel(factor(relocation_treatment), ref = "Control"),
        male = sex == "Male",
        educ_high = ISCED == "High",
        educ_mid = ISCED == "Mid",
        educ_low = ISCED == "Low",
        across(where(is.logical), as.numeric)
    )


# ----------------------------------------------------
# Table 1. Illustrative example
# ----------------------------------------------------

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
    ) %>%
    dplyr::select(
        Category = Category,
        "Country A (absolute)" = country_a,
        "Country A (per 100k)" = per_capita_a,
        "Country B (absolute)" = country_b,
        "Country B (per 100k)" = per_capita_b,
        "Total" = total
    )

caption_example <- paste(
    "Illustrative example of asylum applications for two hypothetical countries"
)

kbl_example <- tbl_example %>% 
    kbl(
        caption = caption_example,
        align = c("l", "r", "r", "r", "r", "r"),
        booktabs = TRUE,
        linesep = "",
    ) %>%
    kable_styling(full_width = TRUE) %>%
    row_spec(0, bold = TRUE) %>%
    column_spec(1, width = "5cm") %>%
    kableExtra::footnote(
        general = "Values formatted for readability. Per capita = per 100,000 population.",
        general_title = "",
        footnote_as_chunk = TRUE
    )

ft_example <- tbl_example %>% 
    flextable(caption_example)

# ----------------------------------------------------
# Table 2. Eurostat averages
# ----------------------------------------------------

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


ft_asylum_rel <- tbl_asylum_rel %>% 
    dplyr::select(
      Country = country,
      "No relocation (per 100k)" = no_relocation,
      "By population (per 100k)" = population,
      "By GDP (per 100k)" = gdp
    ) %>%
    flextable("Allocation rules per capita")

# ----------------------------------------------------
# Table 3. Random Assignment
# ----------------------------------------------------

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
    kableExtra::column_spec(1, bold = TRUE)

ft_assignment <- tbl_assignment %>% 
    as.data.frame.table() %>%
    flextable("Random assignment by country")

# ----------------------------------------------------
# Table 4. Covariate Balance
# ----------------------------------------------------

result <- lapply(names(ds), function(x) {
    column <- ds[[x]]
      
    ## Skip non-numeric or all-missing variables
    if (!is.numeric(column) || all(is.na(column))) return(NA)

    # Regression formula 
    formula <- reformulate(c("relocation_treatment", "country"), x)
    fit <- lm(formula, data = ds, contrasts = list(country = "contr.sum"))
    
    ## compute pooled SD
    group_sds <- tapply(column, ds$relocation_treatment, sd, na.rm = TRUE)
    pooled_sd <- sqrt(sum(group_sds^2) / length(group_sds))

    broom::tidy(fit) |>
        dplyr::mutate(pooled_sd = pooled_sd)
})
names(result) <- names(ds)

tbl_covariates <- result[sapply(result, is.data.frame)] %>%
    dplyr::bind_rows(.id = "variable") %>%

    ## Keep intercepts + treatment effects; drop time variables
    filter(
        grepl("treat|Intercept", term),
        !grepl("^tQ|^tD|^tI", variable)
    ) %>%
    
    ## Format estimates and standardized estimates (Cohen's d–style)
    mutate(
        stat = sprintf("%.2f [%.2f]", estimate, estimate/pooled_sd)
    ) %>% 
    select(term, variable, stat) %>% 
    tidyr::pivot_wider(names_from = term, values_from = stat) %>% 
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
            "political_right" ~ "Political right (0-10)",
            "share_legal_migrants" ~ "Share legal migrants",
            "asylum_decisions_objective" ~ "Objective Asylum decisions (0-10)",
            "asylum_knowledge" ~ "Knowledge of asylum System (1 = no, 4 = high)",
            "asylum_distr_even" ~ "Perceived evenness of AS distribution in EU (0-10)",
            "fair_share" ~ "Perceived AS in own country (1 = too few, 3 = too many)",
            "burden_sharing_important" ~ "Importance of burden sharing (0-10)",
            "equal_distribution_important" ~ "Importance of equal distribution (0-10)",
            "equal_treatment_important" ~ "Importance of equal treatment (0-10)",
            "equal_rights_important" ~ "Importance of equal rights (0-10)",
            "quick_processing_important" ~ "Importance of quick process (0-10)",
            "asylum_fraud_detected" ~ "Frauds detection ability (0-10)",
            "Duration" ~ "Time to complete (mins)",
            .default = variable
        )
    ) %>%

    ## Remove technical variables not intended for publication
    filter(!variable %in% c("weight", "restart_count", "number_accepted_refugees")) %>%

    ## Group variables
    dplyr::mutate(
        group = dplyr::case_when(
            variable %in% c("Age (years)", "Male (%)",
                            "High education (%)","Mid education (%)","Low education (%)") ~ "Sociodemographics",
            variable %in% c("Trust others (0-10)", "Trust in the EU (0-10)",
                            "Trust in government (0-10)", "Political right (0-10)") ~ "Political attitudes",
            variable %in% c("Knowledge of asylum system (1-4)",
                            "Fraud detection ability (0-10)") ~ "Knowledge",
            TRUE ~ "Other"
        )
    ) %>%
    arrange(desc(group))


kbl_covariates <- tbl_covariates |>
    dplyr::select(
        Variable = variable,
        "Control" = "(Intercept)",
        "Diff. Relative" = "relocation_treatmentRelative",
        "Diff. Absolute" = "relocation_treatmentAbsolute",
    ) |>
    kableExtra::kbl(booktabs = TRUE) |>
    kableExtra::kable_classic(full_width = FALSE) |>
    kableExtra::pack_rows("Sociodemographics", 1, 5) |>
    kableExtra::pack_rows("Political attitudes", 6, 5 + 4) |>
    kableExtra::pack_rows("Other", 1 + 5 + 4, nrow(tbl_covariates)) 


# ----------------------------------------------------
# Save all tables
# ----------------------------------------------------

raw_tables <- mget(ls(pattern = "^tbl_"))
latex_tables <- mget(ls(pattern = "^kbl_"))
word_tables <- mget(ls(pattern = "^ft_"))

tables <- list(
  raw = raw_tables,
  latex = latex_tables,
  word = word_tables,
  meta = list(
    created = Sys.time(),
    count_raw = length(raw_tables),
    count_latex = length(latex_tables),
    count_word = length(word_tables)
  )
)



saveRDS(tables, file.path(tab_dir, "descriptives_all_tables.rds"))
message("Tables saved ", file.path(tab_dir, "descriptives_all_tables.rds"))