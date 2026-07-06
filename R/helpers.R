clean_data <- function(data) {
    data |>
        dplyr::rename(
            most_important_goal = most_important,
            treatment = relocation_treatment
        ) |>
        dplyr::mutate(
            respondent_id = dplyr::row_number(),
            parents_abroad = dplyr::case_when(
                country_mother != country ~ "Abroad",
                country_father != country ~ "Abroad",
                TRUE ~ "Local"
            ),
            nuts = dplyr::coalesce(
                nuts_FR, nuts_DE, nuts_BG, nuts_EL,
                nuts_IT, nuts_PL, nuts_ES, nuts_SE
            ),
            dplyr::across(
                dplyr::starts_with("support_"),
                ~ factor(., c("Strongly oppose", "Oppose", "Neutral", "Support", "Strongly support"))
            ),
            dplyr::across(
                c(
                    dplyr::starts_with(c("trust", "political")),
                    dplyr::ends_with("_important"),
                    "asylum_decisions_objective",
                    "asylum_fraud_detected",
                    "asylum_distr_even"
                ),
                to_numeric
            )
        )
}


pivot_data <- function(data) {
    data |>
        tidyr::pivot_longer(
            cols = c(
                no_relocation_ranking,
                relocation_population_ranking,
                relocation_GDP_ranking
            ),
            names_to = "alt",
            names_pattern = "(.*)_ranking",
            values_to = "rank",
        ) |>
        dplyr::mutate(
            rank = as.numeric(gsub("[^0-9]", "", rank)),
            country = factor(country),
            treatment = factor(treatment),
            alt = factor(alt)
        )
}


load_params <- function(file = "config.yml") {
    yaml::read_yaml(file)
}


to_numeric <- function(x) {
    as.numeric(gsub("[^0-9]+", "", x))
}


read_rds <- function(path) {
    logger::log_info("Reading data from {path}")
    base::readRDS(path)
}

save_rds <- function(data, path) {
    logger::log_info("Saving data to {path}")
    base::saveRDS(data, path)
}

save_plot <- function(png_path, ...) {
    pdf_path <- tools::file_path_sans_ext(png_path) |> 
        paste0(".pdf")

    ggplot2::ggsave(
        png_path,
        dpi = 300,
        ...
    )
    logger::log_info(
        "Saved plot {png_path}"
    )
    ggplot2::ggsave(
        pdf_path,
        ...
    )
    logger::log_info(
        "Saved plot painture {pdf_path} 📊"
    )
}


fit_model <- function(formula, family, data, file=NULL, ...) {

    if (is.null(file)) {
        file <- deparse1(formula) |>
            stringr::str_replace_all("\\s+", "") |>
            stringr::str_replace("~", "__") |>
            stringr::str_replace_all("\\*", "_X_") |>
            stringr::str_replace_all("[^[:alnum:]_]+", "_") |>
            stringr::str_remove("_$") |>
            paste0(".rds")
    }

    brms::brm(
        formula = formula,
        data = data,
        family = family,
        file = here::here("output", "models", file),
        file_refit = "on_change",
        cores = 4,
        chains = 4,
    )
}



extract_conditional_effects <- function(object, ...) {
    brms::conditional_effects(object, plot = FALSE, ...)[[1]]
}
