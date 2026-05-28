log_info <- function(...) {
    message(sprintf("[%s] INFO: %s", Sys.time(), paste0(..., collapse = "")))
}

log_warn <- function(...) {
    message(sprintf("[%s] WARN: %s", Sys.time(), paste0(..., collapse = "")))
}

log_error <- function(...) {
    message(sprintf("[%s] ERROR: %s", Sys.time(), paste0(..., collapse = "")))
}

log_start <- function(...) {
    message(sprintf("[%s] START: %s", Sys.time(), paste0(..., collapse = "")))
}

log_done <- function(...) {
    message(sprintf("[%s] Done: %s", Sys.time(), paste0(..., collapse = "")))
}