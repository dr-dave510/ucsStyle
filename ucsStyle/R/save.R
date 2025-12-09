#' Save plot with UCS defaults (1500 x 900 px)
#' @export
ucs_save <- function(plot = NULL,
                     filename = NULL,
                     type = c("png", "jpg", "svg", "pdf"),
                     width_px = 1500,
                     height_px = 900,
                     dpi = 300,
                     bg = "white",
                     scale = 1) {
  type <- match.arg(type)
  width_in  <- width_px / dpi
  height_in <- height_px / dpi
  dev <- switch(type,
    png = ragg::agg_png,
    jpg = ragg::agg_jpeg,
    svg = svglite::svglite,
    pdf = ggplot2::pdf
  )
  if (is.null(filename)) filename <- paste0("plot_", type)
  ext <- tolower(tools::file_ext(filename))
  if (ext == "") {
    filename <- paste0(filename, ".", type)
  } else if (ext != type) {
    filename <- sub(paste0("\\.", ext, "$"), paste0(".", type), filename)
  }
  if (is.null(plot)) plot <- ggplot2::last_plot()
  if (type %in% c("png", "jpg")) {
    ggplot2::ggsave(filename, plot = plot, device = dev,
                    width = width_px, height = height_px, units = "px",
                    dpi = dpi, bg = bg, scale = scale)
  } else if (type == "svg") {
    ggplot2::ggsave(filename, plot = plot, device = dev,
                    width = width_in, height = height_in, units = "in",
                    bg = bg, scale = scale)
  } else {
    ggplot2::ggsave(filename, plot = plot, device = dev,
                    width = width_in, height = height_in, units = "in",
                    bg = bg)
  }
  invisible(filename)
}
