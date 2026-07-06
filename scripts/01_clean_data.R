# ----- setup
library(dplyr)
source(file.path("R", "helpers.R"))

# ---- load
ds_raw <- file.path("data", "raw", "fairness_survey.rds") |>
    read_rds()

# ---- clean
ds_clean <- ds_raw %>%
    dplyr::rename(
        most_important_goal = most_important
    ) %>%
    dplyr::mutate(

        respondent_id = seq_len(dplyr::n()),

        # Parents abroad
        parents_abroad = dplyr::case_when(
            country_mother != country ~ "Abroad",
            country_father != country ~ "Abroad",
            TRUE ~ "Local"
        ),

        ## Combine nuts location into one colum
        nuts = dplyr::coalesce(
            nuts_FR, nuts_DE, nuts_BG, nuts_EL, 
            nuts_IT, nuts_PL, nuts_ES, nuts_SE
        ),

        ## Order factor levels
        dplyr::across(
            dplyr::starts_with("support_"),
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

# ---- save
ds_clean %>%
    save_rds(file.path("data", "processed", "fairness_survey_clean.rds"))
