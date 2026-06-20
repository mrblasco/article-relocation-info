run_script <- function(filename) {
    logger::log_info("Running {filename}")
    source(filename)
}

files <- c("01_clean_data.R", "02_process_data.R", "03_analysis_rankings.R")
for (file in files ) {
    run_script(here::here("scripts", file))
}

