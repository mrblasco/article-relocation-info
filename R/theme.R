require(ggplot2)
require(ggtext)

dodge_value <- 0.5
palette <- "Dark2"

theme_set(theme_minimal())
theme_update(
  plot.title = element_text(size = 12, face = "bold"),
  plot.subtitle = element_text(size = 10),
  plot.caption = element_text(size = 8),

  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),

  legend.position = "none",
  plot.margin = margin(1, 1, 1, 1, unit = "lines"),

  strip.text = element_text(face = "bold"),
  strip.text.y = element_blank(),

  axis.title = element_blank(),
  axis.text.y = ggtext::element_markdown(face = "bold", hjust = 0),
  axis.text.x = element_blank(),
)

update_geom_defaults("point", list(size = 3))

