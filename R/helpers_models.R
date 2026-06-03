library(brms)
library(purrr)
library(dplyr)

fit_brm <- function(data, formula, cores, file, ...) {
    if (!dir.exists(dirname(file))) {
        dir.create(dirname(file), recursive = TRUE)
    }

    brms::brm(data = data, formula = formula, cores = cores, file = file, ...)
}

fit_brm_grouped <- function(data, group_var, formula, file_prefix, cores = 4) {
    groups <- na.omit(unique(data[[group_var]]))

    log_info("Fitting grouped model: ", length(groups), " groups")

    set_names(groups) %>%
        map(function(g) {
            start <- Sys.time()
            log_start(paste("group:", g))

            fit <- filter(data, .data[[group_var]] == g) |>
                fit_brm(
                    formula = formula,
                    cores = cores,
                    file = sprintf("%s_%s.rds", file_prefix, g),
                    file_refit = "on_change"
                )

            log_done(paste("group:", g), start)
            fit
        })
}



make_grid <- function(data) {
    expand_grid(
        asylum_applications_z = seq(
            min(data$asylum_applications_z, na.rm = TRUE),
            max(data$asylum_applications_z, na.rm = TRUE),
            length.out = 50
        ),
        treatment = c("No info", "Absolute"),
        alt = unique(data$alt)[3]
    )
}


predict_model <- function(object, grid) {
    fitted(object, newdata = grid, re_formula = NA, summary = TRUE) |>
        as.data.frame() |>
        dplyr::bind_cols(grid)
}

predict_models <- function(models, grid, groups) {
    map_dfr(groups, \(g) predict_model(models[[g]]) |> mutate(group = g))
}


replace_terms <- function(df) {
    df %>%
        mutate(
            grp = case_when(
                grepl("^alt", term) ~ "Alternative",
                grepl("^treatment", term) ~ "Treatment",
                grepl("^country", term) ~ "Country",
                TRUE ~ "Other"
            ),

            term = replace_values(
                term,
                "altPopulation" ~ "Relocation by population",
                "altNorelocation" ~ "No relocation",
                "treatmentAbsolute" ~ "Absolute",
                "treatmentRelative" ~ "Relative",
                "treatmentNoinfo" ~ "No info",
                "asylum_applications_z" ~ "Asylum applications (z)",
                "treatmentNoinfo:asylum_applications_z" ~ "No info x asylum applications",
                "treatmentAbsolute:asylum_applications_z" ~ "Absolute x asylum applications",
                "treatmentRelative:asylum_applications_z" ~ "Relative x asylum applications"
            )
        )
}
