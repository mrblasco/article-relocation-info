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

save_plot <- function(filename, ...) {
    ggplot2::ggsave(
        filename = here::here(
            "output", "figures",
            filename
        ),
        ...
    )
}

extract_conditional_effects <- function(object, ...) {
    brms::conditional_effects(object, plot = FALSE, ...)[[1]]
}
