#' utils for flextable


make_flextable <- function(tbl, caption) {
    tbl %>%
        flextable::flextable() %>%
        flextable::set_caption(caption) %>%
        flextable::bold(part = "header") %>%
        flextable::autofit()
}


make_kable <- function(tbl, caption, align = NULL, footnote_text = NULL, ...) {
    kbl_tbl <- tbl %>%
        kableExtra::kbl(
            caption = caption,
            align = align,
            booktabs = TRUE,
            linesep = "",
            ...
        ) %>%
        kableExtra::kable_styling(full_width = TRUE) %>%
        kableExtra::row_spec(0, bold = TRUE)
  
  if (!is.null(footnote_text)) {
    kbl_tbl <- kbl_tbl %>%
      kableExtra::footnote(
        general = footnote_text, 
        general_title = "", 
        footnote_as_chunk = TRUE
    )
  }
  
  kbl_tbl
}

