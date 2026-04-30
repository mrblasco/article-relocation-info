log_info <- function(...) {
message(sprintf("[%s] INFO: %s", Sys.time(), paste0(..., collapse = "")))
}

log_warn <- function(...) {
message(sprintf("[%s] WARN: %s", Sys.time(), paste0(..., collapse = "")))
}

log_error <- function(...) {
message(sprintf("[%s] ERROR: %s", Sys.time(), paste0(..., collapse = "")))
}
