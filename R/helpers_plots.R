library(ggplot2)
library(dplyr)

save_plot <- function(plot, filename, formats = NULL, ...) {
    log_info("Saving plot to ", filename)

    save_one <- function(file) {
        ggplot2::ggsave(
            filename = file,
            plot = plot,
            ...
        )
    }

    if (is.null(formats)) {
        save_one(filename)
        return(invisible(NULL))
    }

    base <- sub("\\.[^.]*$", "", filename)

    for (fmt in formats) {
        file <- paste0(base, ".", fmt)
        log_info("Saving image to ", file)
        save_one(file)
    }

    invisible(NULL)
}



plot_coef_base <- function(data, palette, title) {
    data |>
        ggplot(aes(
            x = estimate,
            y = term,
            xmin = conf.low,
            xmax = conf.high
        )) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
        geom_pointrange(position = position_dodge(0.5)) +
        scale_color_manual(values = palette) +
        labs(x = title, y = NULL)
}

plot_coef <- function(df, group_levels, palette, title = NULL) {
    df %>%
        filter(effect == "fixed", !grepl("Intercept", term)) %>%
        mutate(group = factor(group, levels = group_levels)) %>%
        ggplot(aes(
            x = estimate,
            y = term,
            xmin = conf.low,
            xmax = conf.high,
            color = group,
            shape = group
        )) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
        geom_pointrange(position = position_dodge(0.5)) +
        scale_color_manual(values = palette) +
        labs(x = title, y = NULL)
}


plot_predictions <- function(df, group_levels, palette, x, y, ci_low, ci_high) {
    df %>%
        mutate(group = factor(group, levels = group_levels)) %>%
        ggplot(aes(
            x = .data[[x]], y = .data[[y]],
            color = group, fill = group, linetype = group
        )) +
        geom_ribbon(aes(ymin = .data[[ci_low]], ymax = .data[[ci_high]]),
            alpha = .12, linewidth = 0
        ) +
        geom_line(linewidth = .9) +
        scale_color_manual(values = palette) +
        scale_fill_manual(values = palette)
}
