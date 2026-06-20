# ----------------------------------------------------
# Prepare dataset for analysis
# Author: Andrea Blasco
# ----------------------------------------------------
suppressMessages({
    library(dplyr)
    library(fairMigrate)
})

data_dir <- file.path("data", "processed")
dir.create(data_dir, showWarnings = FALSE)

ds_survey_filename <- file.path(data_dir, "fair_survey_clean.rds")
ds_long_filename <- file.path(data_dir, "fair_survey_long.rds")

# ----------------------------------------------------
# Load data from `fairMigrate` package
# ----------------------------------------------------

message("Loading raw survey data...")
ds_raw <- fairMigrate::fairness_survey
message("Raw data loaded: ", nrow(ds_raw), " rows, ", ncol(ds_raw), " columns")

ds_survey <- ds_raw %>%
    dplyr::rename(most_important_goal = most_important) %>%
    dplyr::mutate(
        ## Combine parents abroad single col
        parents_abroad = case_when(
            country_mother != country ~ "Abroad",
            country_father != country ~ "Abroad",
            TRUE ~ "Local"
        ),

        ## Combine nuts location into one colum
        nuts = dplyr::coalesce(
            nuts_FR, nuts_DE, nuts_BG, nuts_EL, 
            nuts_IT, nuts_PL, nuts_ES, nuts_SE
        ),

        ## Factor 
        dplyr::across(
            starts_with("support_"),
            ~ factor(., c("Strongly oppose", "Oppose", "Neutral", "Support", "Strongly support"))
        ),

        ## Convert 0-10 scales to numeric
        dplyr::across(
            c(
                dplyr::starts_with(c("trust", "political")), 
                dplyr::ends_with("_important"),
                "asylum_decisions_objective",
                "asylum_fraud_detected",
                "asylum_distr_even"
            ),
            ~ gsub("[^0-9]+", "", .) %>% as.numeric()
        )
    )
message("Processed data: ", nrow(ds_survey), " rows, ", ncol(ds_survey), " columns")

# ----------------------------------------------------
# Convert rankings to long format
# ----------------------------------------------------

cols <- grep("^(no|rel).*_ranking", names(ds_survey), value = TRUE)

ds_long <- ds_survey %>%
    dplyr::mutate(respondent_id = dplyr::row_number()) %>%

    ## Pivot rankings 
    tidyr::pivot_longer(
        dplyr::all_of(cols),
        names_to = "alt",
        values_to = "choice"
    ) %>%

    ## Recode information treatment and relocation appraoch
    dplyr::mutate(
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
message("Long-format data created: ", nrow(ds_long), " rows")


# ----------------------------------------------------
# Save ... 
# ----------------------------------------------------
saveRDS(ds_long, ds_long_filename)
saveRDS(ds_survey, ds_survey_filename)
message("Datasets saved to: ", ds_survey_filename, " and ", ds_long_filename)
