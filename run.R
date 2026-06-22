# ---- helpers ----
run_script <- function(path) {
    logger::log_info("Running {path}...")
    tryCatch(
        source(path), 
        error = function(e) {
            logger::log_error("Error {e}")
            stop()
        }
    )
    logger::log_info("Done!")
}


#run_script("scripts/_import_data.R")
run_script("scripts/01_clean_data.R")
run_script("scripts/02_process_data.R")
run_script("scripts/03_analysis_top_ranks.R")