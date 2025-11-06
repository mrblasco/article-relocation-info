# ----- setup, include = FALSE
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(knitr)
  library(kableExtra)
  library(fairMigrate)
  library(mlogit)
  library(openxlsx)
  library(broom)
})


is_latex <- knitr::is_latex_output()

opts_chunk$set(
    error = TRUE,
    echo = FALSE,
    dev = ifelse(is_latex, "pdf", "png"),
    dpi = 600
)

theme_set(theme_minimal(base_size = 12, base_family = "Helvetica"))
theme_update(
    panel.background = element_rect(color = 'gray85'),
    #panel.grid.major.y = element_blank(),
    #panel.grid.minor.y = element_blank(),
    #panel.grid.major.x = element_line(color = "gray85", linewidth = 0.3),
    strip.background = element_rect(fill = "gray95", color = NA),
    strip.text = element_text(face = "bold", size = 11, hjust = 0),
    axis.text = element_text(color = "black", size = 10),
    axis.title.x = element_text(size = 11, margin = margin(t = 6)),
    axis.title.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    plot.caption = element_text(size = 9, hjust = 0, color = "gray40", margin = margin(t = 10)),
    plot.margin = margin(1, 1, 1, 1, unit = "lines"),
)


# ---- helper: pretty table ----
pretty_table <- function(...) {
  istex <- knitr::is_latex_output()
  kableExtra::kbl(..., booktabs = istex) %>%
    kableExtra::kable_styling() %>%
    kableExtra::column_spec(1, bold = TRUE)
}

# ======================================================
#   FUNCTIONS
# ======================================================

load_data <- function(path) {
    raw_data <- readRDS(path)
    df <- raw_data %>%
        dplyr::rename(educ = ISCED)
    
    df <- df %>%
        dplyr::mutate(
            asylum_knowledge = case_when(
                grepl("basic|no knowledge", asylum_knowledge) ~ "Basic or no knowledge", 
                grepl("good|great", asylum_knowledge) ~ "Good or great knowledge", 
            ),
            equal_distribution_important = equal_distribution_important %>%
                as.numeric(gsub("[^0-9]+", "", .)) %>% 
                cut(breaks = c(0, 3, 6, 10), c("3-Low", "2-Mid", "1-High")),
            fair_share = case_when(
                grepl("too low|Yes", fair_share) ~ "Fair share or too low",
                TRUE ~ fair_share
            )
        )
    df
}

fit_models <- function(data, group_var, treatment_var, model_formula) {
    groups <-unique(data[[group_var]]) %>%
    setdiff(c("Other", "Refusal", "Prefer not to answer", "I don’t know", "Unknown"))
  
    conditions <- unique(data[[treatment_var]])

    results <- list()
    for (g in groups) for (cond in conditions) {
        subset_data <- filter(data, .data[[group_var]] == g,
                                .data[[treatment_var]] == cond)
        tryCatch({
            fit <- mlogit(model_formula, subset_data)
            results[[paste(cond, g, sep = "__")]] <- tidy(fit, conf.int = TRUE)
        }, error = \(.) message(sprintf("Couldn't estimate %s = %s", group_var, g)))
    }
    results
}

process_coefficients <- function(coeff_list, group_var) {
    coeffs <- bind_rows(coeff_list, .id = "info") %>%
    mutate(
        group = sub(".*__", "", info),
        condition = sub("__.*", "", info),
        term = term %>%
            gsub(".Intercept.[:]", "", .) %>%
            gsub("relocation_(.*)_ranking", "Relocation by \\1 (vs. no relocation)", .) %>%
            gsub("scale.asyl_skrs_rel.", "Asylum seekers (1 SD change)", .),
    )

  label_map <- list(
    trust_eu_cut = c("low" = "Low trust", "middle" = "Moderate trust", "high" = "High trust"),
    educ = c("Low" = "Low education", "Mid" = "Medium education", "High" = "High education"),
    political_right_cut = c("left" = "Left", "neutral" = "Center", "right" = "Right")
  )

  if (group_var %in% names(label_map)) {
    coeffs <- mutate(coeffs,
      group = factor(group,
                     levels = names(label_map[[group_var]]),
                     labels = label_map[[group_var]]))
  }
  coeffs
}

plot_coeffs <- function(coeffs) {
    ggplot(coeffs, aes(
        x = estimate, xmin = conf.low, xmax = conf.high, y = group,
        color = condition, shape = condition
    )) +
    facet_wrap(~term, ncol = 1, scales = "free_y") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_linerange(position = position_dodge(0.5)) +
    geom_point(position = position_dodge(0.5), size = 2.5) +
    scale_color_brewer(palette = "Set1") +
    labs(
        x = "Estimate (95% CI)"
    ) +
    annotate("text", x = Inf, y = 0.5, label = "More fair", hjust = 1.5, size = 2.5) +
    annotate("text", x = -Inf, y = 0.5, label = "Less fair", hjust = -0.5, size = 2.5)
}


# ======================================================
#   Main
# ======================================================

data_path <- file.path(
    "data", 
    "processed", "fairness_survey_idx.rds"
)

d2 <- load_data(data_path)

model <- rank ~ 0 + scale(asyl_skrs_rel) | 1

vars <- c("sex", "age_cat", "educ", "fair_share",
          "political_right_cut", "trust_eu_cut", 
          "asylum_knowledge", "parents_abroad")

results <- lapply(vars, function(var) {
  fit <- fit_models(d2, var, "relocation_treatment", model)
  coeffs <- process_coefficients(fit, var)
  plot <- plot_coeffs(coeffs)
  
  list(
    var = var,
    fit = fit,
    coeffs = coeffs,
    plot = plot
  )
})

names(results) <- vars


# ---- plots, fig.asp = 1 ----  

lapply(results, function(.) .$plot)


# ---- save-table ------------------------------------

wb <- createWorkbook()

for (nm in names(results)) {
  addWorksheet(wb, nm)
  writeData(wb, nm, results[[nm]]$coeffs)
}
saveWorkbook(wb, "tbl_export.xlsx", overwrite = TRUE)



# ---- session-info
sessionInfo()
