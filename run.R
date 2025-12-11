#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(rmarkdown))

# Parse command-line args
args <- commandArgs(trailingOnly = TRUE)

# Check for --all flag
render_all <- any(args %in% c("--all", "-a"))

# Perform the render
if (render_all) {
  message("Rendering ALL formats...")
  out <- render("main.Rmd", output_dir = "docs", output_format = "all")
} else {
  message("Rendering DEFAULT format...")
  out <- render("main.Rmd", output_dir = "docs")
}

# Open the generated file (macOS)
system(paste("open", out))

message("Done. Output: ", out)
