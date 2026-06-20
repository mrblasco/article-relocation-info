suppressMessages({
    library(dplyr)
    library(here)
})


# ---- load 
ds_raw <- readRDS(
    here::here(
        "data", "raw", "fairness_survey.rds"
    )
)

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

# ---- save 
filename <- here::here(
    "data", "processed", 
    "fairness_survey_clean.rds"
)
saveRDS(
    ds_clean,
    filename
)
logger::log_info("Cleaned data {filename}")
