
import_data <- function(path) {
    dir.create(
        dirname(path),
        recursive = TRUE,
        showWarnings = FALSE
    )

    saveRDS(
        fairMigrate::fairness_survey,
        path
    )
    logger::log_info(
        "Imported data into {path}"
    )
    invisible(path)
}

import_data(
    file.path(
        "data", "raw", "fairness_survey.rds"
    )
)

