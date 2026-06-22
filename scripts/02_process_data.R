suppressMessages({
    library(dplyr)
    library(tidyr)
})

# --- load
ds_survey <- readRDS(
    here::here(
        "data", "processed",
        "fair_survey_clean.rds"
    )
)

# ---- asylum stats
tbl_asylum_rel <- data.frame(
    country = c(
        "Germany", "Spain", "Greece", "Bulgaria",
        "France", "Sweden", "Italy", "Poland"
    ),
    no_relocation = c(313, 138, 397, 178, 151, 344, 128, 16),
    population = c(169, 169, 169, 169, 169, 169, 169, 169),
    gdp = c(219, 136, 95, 65, 184, 230, 158, 92)
) %>%
    mutate(
        country_type = case_when(
            no_relocation > population ~ "Net sender",
            no_relocation < population ~ "Net receiver",
            TRUE ~ "Other"
        )
    )

# ---- long format
ds_long <- ds_survey %>%
    mutate(respondent_id=row_number())%>%
    tidyr::pivot_longer(
        cols = c(
            no_relocation_ranking,
            relocation_population_ranking,
            relocation_GDP_ranking
        ),
        names_to = "alt",
        names_pattern = "(.*)_ranking",
        values_to = "rank",
    ) %>%
    dplyr::rename(
        treatment = relocation_treatment
    ) %>%
    dplyr::mutate(
        rank = as.numeric(gsub("[^0-9]", "", rank)),
        country = factor(country),
        treatment = factor(treatment),
        alt = factor(alt)
    )

ds_asylum_applications <- tbl_asylum_rel %>%
    tidyr::pivot_longer(
        c("population", "gdp", "no_relocation"),
        names_to = "alt",
        values_to = "asylum_applications"
    ) %>%
    dplyr::mutate(
        alt = dplyr::replace_values(
            alt,
            "gdp" ~ "relocation_GDP",
            "population" ~ "relocation_population",
        )
    ) %>%
    dplyr::right_join(
        ds_long,
        by = c("alt", "country")
    ) %>%
    dplyr::mutate(
        asylum_applications_z = scale(asylum_applications)[, 1],
        rank = factor(rank, ordered = TRUE)
    )

dplyr::glimpse(ds_asylum_applications)

# ---- save 
filename <- here::here(
    "data", "processed",
    "fairness_survey_long.rds"
)
saveRDS(ds_asylum_applications, filename)

logger::log_info("Saved long format {filename}")
