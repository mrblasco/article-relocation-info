suppressMessages({
    library(dplyr)
    library(tidyr)
    library(ggplot2)
    library(patchwork)
    library(yaml)
    library(knitr)
    library(kableExtra)
    library(fairMigrate)
    library(mlogit)
})

# ----------------------------------------------------
# Paths
# ----------------------------------------------------

tab_dir <- file.path("results", "tables")
fig_dir <- file.path("results", "figures")
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
# Multinomial logit data preparation
# ----------------------------------------------------

ds_mlogit <- ds_combined %>%
    ## workaround to allow `left_join`
    dplyr::mutate(dfidx_resp_id = respondent_id) %>%
    dplyr::select(
        respondent_id, dfidx_resp_id, alt, choice = rank, edu = ISCED, 
        info, country, asyl_skrs_rel
    ) %>% 
    mutate(across(where(is.character), as.factor)) %>%
    dfidx::dfidx(
        idx = c("dfidx_resp_id", "alt"),
        choice = "choice",
        ranked = TRUE
    )


# ----------------------------------------------------
# Baseline
# ----------------------------------------------------

model <- choice ~ scale(asyl_skrs_rel) | info | 0
fit_base <- mlogit(model, ds_mlogit)

tbl_base_coeffs <- broom::tidy(fit_base, conf.int = TRUE) 

# ----------------------------------------------------
# by treeatment
# ----------------------------------------------------

treatments <- unique(ds_mlogit$info)
model <- choice ~ 0 + scale(asyl_skrs_rel) | 1

coeff_list <- list()
for (j in treatments) {
  fit <- mlogit(model, ds_mlogit, subset = info == j)
  coeff_list [[j]] <- broom::tidy(fit, conf.int = TRUE, exponentiated = TRUE)
}

tbl_group_coeffs <- bind_rows(coeff_list, .id = "relocation_treatment")

# ----------------------------------------------------
# Heterogeneous effects
# ----------------------------------------------------

fit_models <- function(data, group_var, treatment_var, model_formula) {
    stopifnot(
        "Group var not found in data" = group_var %in% colnames(data),
        "Treatment var not found in data" = treatment_var %in% colnames(data)
    )

    groups <- unique(data[[group_var]]) %>%
        setdiff(c("Other", "Refusal", "Prefer not to answer", "I don’t know", "Unknown"))
    
    conditions <- unique(data[[treatment_var]])
    
    results <- list()
    for (g in groups) for (cond in conditions) {
        subset_data <- dplyr::filter(
            data,
            .data[[group_var]] == g,
            .data[[treatment_var]] == cond
        )
        tryCatch({
            fit <- mlogit(model_formula, subset_data)
            coeffs <- broom::tidy(fit, conf.int = TRUE)
            coeffs$group_var <- group_var
            coeffs$group <- g
            coeffs$condition <- cond
            results[[paste(cond, g, sep = "__")]] <- coeffs
        }, error = \(.) message(sprintf("Couldn't estimate %s = %s", group_var, g)))
    }
    results
}

fit_treatment <- function(data, group_var, ...) {
    result <- fit_models(
        data = data, 
        group_var = group_var, 
        treatment_var = "info", 
        model_formula = choice ~ 0 + asyl_skrs_rel_scaled | 1
    )
    dplyr::bind_rows(result, .id = "model")
}

ds_mlogit_full <- ds_mlogit %>%
    dplyr::left_join(
        dplyr::distinct(
            ds_long,
            respondent_id, sex, 
            age_cat, fair_share, 
            trust_eu, trust, trust_gov,
            asylum_knowledge, political_right,
            equal_distribution_important,
            fair_share
        ),
        by = "respondent_id"
    ) %>%
    dplyr::mutate(
        asyl_skrs_rel_scaled = scale(asyl_skrs_rel),
        
        ## cut trusts in 3 categories
        across(starts_with("trust"), function(.) {
            cut(., breaks = c(0, 3, 6, 10), c("3-Low", "2-Mid", "1-High"),
                include.lowest = TRUE)
        }),

        ## Aggregate knowledge responses into two categories
        asylum_knowledge = case_when(
            grepl("basic|no knowledge", asylum_knowledge) ~ "Basic or no knowledge", 
            grepl("good|great", asylum_knowledge) ~ "Good or great knowledge", 
        ),

        ## Cut ploticial right 
        political_right = cut(
            political_right, 
            c(0, 3, 6, 10), 
            include.lowest = TRUE, 
            labels = c("left", "neutral", "right")
        ),

        ## Cut distribution in 3 categories
        equal_distribution_important = equal_distribution_important %>%
            cut(breaks = c(0, 3, 6, 10), c("3-Low", "2-Mid", "1-High"),
                include.lowest = TRUE),
        
        ## Aggregage fair share
        fair_share = case_when(
            grepl("too low|Yes", fair_share) ~ "Fair share or too low",
            TRUE ~ fair_share
        )
    )


vars <- c("age_cat", "sex", "trust_eu", "political_right", "fair_share", "asylum_knowledge")
tbl_coeffs_hetero <- lapply(vars, fit_treatment, data = ds_mlogit_full) %>%
    dplyr::bind_rows()

# ----------------------------------------------------
# save
# ----------------------------------------------------

tbl_base_coeffs$relocation_treatment <- "All"
tbl_coeffs <- bind_rows(tbl_group_coeffs, tbl_base_coeffs)

saveRDS(tbl_coeffs, file.path(tab_dir, "rologit_coeffs.rds"))
saveRDS(tbl_coeffs_hetero, file.path(tab_dir, "rologit_coeffs_hetero.rds"))

message("Saved ", file.path(tab_dir, "rologit_coeffs.rds"))
