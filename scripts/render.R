#!/usr/bin/env Rscript

suppressPackageStartupMessages({
    library(argparse)
    library(yaml)
    library(rmarkdown)
    library(logger)
})

# ---------------------------
# Logger setup (color + optional file logging)
# ---------------------------
log_layout(layout_glue_colors)
log_appender(appender_console)
log_threshold(INFO)

# ---------------------------
# Argument parser
# ---------------------------
parser <- ArgumentParser(description = "Flexible RMarkdown rendering CLI")

parser$add_argument("-i", "--input",
    default = "main.Rmd",
    help = "Input Rmd file"
)

parser$add_argument("-f", "--format",
    default = "bookdown::html_document2",
    help = "Output format"
)

parser$add_argument("-p", "--params",
    default = "config/_title.yml",
    help = "YAML params file"
)

parser$add_argument("-o", "--output_dir",
    default = NULL,
    help = "Output directory (default: same as input)"
)

parser$add_argument("-n", "--output_file",
    default = NULL,
    help = "Custom output file name"
)

parser$add_argument("--output_yaml",
    default = NULL,
    help = "Custom yaml output file name"
)

parser$add_argument("--encoding",
    default = "UTF-8",
    help = "File encoding"
)

parser$add_argument("--knit_root",
    default = NULL,
    help = "Root directory for knitting"
)

parser$add_argument("--clean_env",
    default = "FALSE",
    help = "Whether to clean R environment before rendering (TRUE/FALSE)"
)

parser$add_argument("--quiet",
    default = "TRUE",
    help = "Suppress render output (TRUE/FALSE)"
)

parser$add_argument("--log_file",
    default = NULL,
    help = "Optional log file path"
)

args <- parser$parse_args()

# Convert logical args
clean_env <- tolower(args$clean_env) == "true"
quiet <- tolower(args$quiet) == "true"

# ---------------------------
# Optional file logging
# ---------------------------
if (!is.null(args$log_file)) {
    log_appender(appender_tee(args$log_file))
    log_info("📝 Logging to file: {args$log_file}")
}

# ---------------------------
# Runtime info
# ---------------------------
start_time <- Sys.time()

log_info("RMarkdown CLI renderer started")
log_info("Input: {args$input}")
log_info("Format: {args$format}")
log_info("Params: {args$params}")

if (!is.null(args$output_dir))
    log_info("Output dir: {args$output_dir}")

if (!is.null(args$output_yaml))
    log_info("Output yaml: {args$output_yaml}")

if (!is.null(args$output_file))
    log_info("Output file: {args$output_file}")



# ---------------------------
# Validate files
# ---------------------------
if (!file.exists(args$input)) stop("Input file missing")
if (!file.exists(args$params)) stop("Params file missing")

# ---------------------------
# Load params
# ---------------------------
params <- yaml::read_yaml(args$params)
log_info("Loaded params ({length(params)} keys)")

# ---------------------------
# Render timing
# ---------------------------
render_start <- Sys.time()

log_info("Rendering started...")

tryCatch(
    {
        rmarkdown::render(
            input = args$input,
            output_format = args$format,
            params = params,
            output_dir = args$output_dir,
            output_file = args$output_file,
            output_yaml = args$output_yaml,
            knit_root_dir = args$knit_root,
            encoding = args$encoding,
            clean = clean_env,
            quiet = quiet
        )

        render_end <- Sys.time()
        duration <- difftime(render_end, render_start, units = "secs")


        log_info("Render completed successfully")
        log_info("⏱Duration: {round(as.numeric(duration), 2)}s")
    },
    error = function(e) {
        log_error("Render failed: {e$message}")
        stop(e)
    }
)

total_time <- difftime(Sys.time(), start_time, units = "secs")
log_info("Total runtime: {round(as.numeric(total_time), 2)}s")
log_info("Done!")
