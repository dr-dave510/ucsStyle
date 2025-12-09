#' UCS ggplot theme for dark backgrounds (Gotham)
#'
#' Sets UCS dark background styling and marks session bg = "dark" for scale logic.
#' @param bg Background fill colour (default "#000000").
#' @param fg Foreground text colour (default "#FFFFFF").
#' @param grid Gridline colour (default "#444444").
#' @param show_x_grid Logical; show major x gridlines (default FALSE).
#' @return A ggplot2 theme object.
#' @export
ucs_style_dark <- function(bg = "#000000", fg = "#FFFFFF", grid = "#444444", show_x_grid = FALSE) {
  options(ucs.bg = "dark")
  font <- "Gotham"
  ggplot2::theme(
    plot.title    = ggplot2::element_text(family = font, size = 28, face = "bold", color = fg),
    plot.subtitle = ggplot2::element_text(family = font, size = 22, color = fg, margin = ggplot2::margin(9, 0, 9, 0)),
    plot.caption  = ggplot2::element_blank(),
    legend.position   = "top",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title      = ggplot2::element_blank(),
    legend.key        = ggplot2::element_blank(),
    legend.text       = ggplot2::element_text(family = font, size = 18, color = fg),
    axis.title   = ggplot2::element_blank(),
    axis.text    = ggplot2::element_text(family = font, size = 18, color = fg),
    axis.text.x  = ggplot2::element_text(margin = ggplot2::margin(5, b = 10), color = fg),
    axis.ticks   = ggplot2::element_blank(),
    axis.line    = ggplot2::element_blank(),
    panel.grid.minor   = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color = grid),
    panel.grid.major.x = if (isTRUE(show_x_grid)) ggplot2::element_line(color = grid) else ggplot2::element_blank(),
    panel.background = ggplot2::element_rect(fill = bg, colour = bg),
    plot.background  = ggplot2::element_rect(fill = bg, colour = bg),
    strip.background = ggplot2::element_rect(fill = bg, colour = bg),
    strip.text       = ggplot2::element_text(size = 22, hjust = 0, color = fg)
  )
}
