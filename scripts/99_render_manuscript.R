library(rmarkdown)
library(bookdown)

source("R/logging.R")

input <- file.path("main.Rmd")
log_info("Input file: ", input)

log_info("Retrieving git branch...")
branch <- tryCatch({
    system("git rev-parse --abbrev-ref HEAD", intern = TRUE)
}, error = function(e) {
    log_error("Failed to get git branch: ", e$message)
    "unknown-branch"
})

branch <- gsub("/", "-", branch)
log_info("Using branch: ", branch)

output_dir <- file.path("draft", branch)
log_info("Output directory: ", output_dir)

log_info("Starting render...")
out <- tryCatch({
    render(
        input,
        output_format = "bookdown::pdf_document2",
        output_dir = output_dir,
        quiet = TRUE
        )
    }, 
    error = function(e) {
        log_error("Render failed: ", e$message)
        stop(e)
})

log_info("Render completed successfully.")
log_info("Output file: ", out)