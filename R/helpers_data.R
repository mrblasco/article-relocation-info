library(here)
library(dplyr)

read_rds <- function(...) {
    path <- here::here(...)
    log_info("Loading data ", path)
    base::readRDS(path)
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
