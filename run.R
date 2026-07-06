logger::log_layout(logger::layout_glue_colors)

dir.create(file.path("output", "models"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path("output", "tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path("output", "figures"), showWarnings = FALSE, recursive = TRUE)

# ---- helpers ----
run_script <- function(path) {
    logger::log_info("🚀 {path}...")
    start <- Sys.time()
    tryCatch(
        source(path),
        error = function(e) {
            logger::log_error("Error {e}")
            stop()
        }
    )
    end <- Sys.time()
    logger::log_info(
        "Elapsed {elapsed} secs",
        elapsed = round(difftime(end, start, units = "secs"), 2)
    )
}


run_script("scripts/_import_data.R")
run_script("scripts/01_clean_data.R")
run_script("scripts/02_process_data.R")
run_script("scripts/03_analysis_top_ranks.R")
run_script("scripts/04_analysis_avg_rank.R")
run_script("_temp.R")


rmarkdown::render(
    "main.Rmd",
    quiet = TRUE
)