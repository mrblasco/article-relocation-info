
theme_nice <- function(base_size = 14) {
    require(ggplot2)
    theme_classic(base_size = base_size) +
        theme(
            plot.title = element_text(
                size = base_size,
                face = "bold"
            ),
            plot.subtitle = element_text(
                size = base_size * 0.8
            ),
            plot.caption = element_text(
                size = base_size * 0.7
            ),

            panel.grid.major = element_line(color = "gray", linewidth = .1),
            panel.grid.minor = element_blank(),

            legend.position = "bottom",
            legend.title = element_blank(),

            plot.margin = margin(
                1, 1, 1, 1,
                unit = "lines"
            ),

            strip.background = element_blank(),
            strip.text = element_text(
                face = "bold"
            ),

            axis.ticks.length = unit(-0.2, "cm"),
            axis.title = element_text(face = "bold", size = 11),
            axis.text = element_text(color = "black"),
        )
}
