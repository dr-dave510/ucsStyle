#' UCS brand palette and scales
#'
#' Named UCS palette and scale helpers that can drop white/black based on context.
#'
#' @export
ucs_palette <- c(
  "Primary Black"     = "#000000",
  "Secondary White"   = "#FFFFFF",
  "Accent Yellow 1"   = "#FFE900",
  "Accent Yellow 2"   = "#FFC600",
  "Accent Orange"     = "#FF522B",
  "Accent Red"        = "#CB2C30",
  "Accent Magenta"    = "#FF0093",
  "Accent Blue 1"     = "#3044B5",
  "Accent Blue 2"     = "#007AA5",
  "Accent Cyan"       = "#00AEEF",
  "Accent Lime 1"     = "#BBDC00",
  "Accent Lime 2"     = "#6EC829"
)

ucs_pal <- function(reverse = FALSE, drop_white = FALSE, drop_black = FALSE, drop_black_if_multi = FALSE) {
  pal <- ucs_palette
  if (isTRUE(drop_white)) pal <- pal[pal != "#FFFFFF"]
  if (isTRUE(reverse)) pal <- rev(pal)
  force(function(n) {
    p <- pal
    if (isTRUE(drop_black) || (isTRUE(drop_black_if_multi) && n > 1)) {
      p <- p[p != "#000000"]
    }
    if (n <= length(p)) {
      unname(p[seq_len(n)])
    } else {
      grDevices::colorRampPalette(unname(p))(n)
    }
  })
}

.ucs_bg_is_dark <- function() identical(getOption("ucs.bg", default = "light"), "dark")

scale_color_ucs <- function(...,
                            reverse = FALSE,
                            drop_white = !.ucs_bg_is_dark(),
                            drop_black = .ucs_bg_is_dark(),
                            drop_black_if_multi = TRUE) {
  ggplot2::discrete_scale("colour", "ucs",
    ucs_pal(reverse = reverse, drop_white = drop_white,
            drop_black = drop_black, drop_black_if_multi = drop_black_if_multi),
    ...)
}

scale_fill_ucs <- function(...,
                           reverse = FALSE,
                           drop_white = FALSE,
                           drop_black = .ucs_bg_is_dark(),
                           drop_black_if_multi = TRUE) {
  ggplot2::discrete_scale("fill", "ucs",
    ucs_pal(reverse = reverse, drop_white = drop_white,
            drop_black = drop_black, drop_black_if_multi = drop_black_if_multi),
    ...)
}

scale_color_ucs_continuous <- function(low = NULL, high = NULL, ...) {
  pal <- unname(ucs_palette)
  ggplot2::scale_color_gradient(low = if (is.null(low)) pal[1] else low,
                                high = if (is.null(high)) pal[length(pal)] else high,
                                ...)
}

scale_fill_ucs_continuous <- function(low = NULL, high = NULL, ...) {
  pal <- unname(ucs_palette)
  ggplot2::scale_fill_gradient(low = if (is.null(low)) pal[1] else low,
                               high = if (is.null(high)) pal[length(pal)] else high,
                               ...)
}
