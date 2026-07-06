library(here)
library(dplyr)

read_rds <- function(path) {
    logger::log_info("Loading data from {path}")
    base::readRDS(path)
}

save_rds <- function(data, path) {
    logger::log_info("Saving data to {path}")
    base::saveRDS(data, path)
}


recode_labels <- function(df) {
    df |>
        dplyr::mutate(
            relocation = dplyr::replace_values(
                relocation,
                "Relocation: by pop" ~ "Population",
                "Relocation: by GDP" ~ "GDP"
            ),
            relocation_treatment = dplyr::replace_values(
                relocation_treatment,
                "Info: Absolute" ~ "Absolute",
                "Info: Relative" ~ "Relative"
            )
        )
}
