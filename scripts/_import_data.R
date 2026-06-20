# it requires library(fairMigrate)
filename <- here::here(
        "data", "raw", "fairness_survey.rds"
    )
saveRDS(
    fairMigrate::fairness_survey,
    filename
)
logger::log_info("Imported data {filename}")