# Set safe defaults on attach (points/lines/bars = primary black)
.onAttach <- function(libname, pkgname) {
  primary <- "#000000"
  ggplot2::update_geom_defaults("point", list(colour = primary))
  ggplot2::update_geom_defaults("line",  list(colour = primary))
  ggplot2::update_geom_defaults("col",   list(fill   = primary))
  ggplot2::update_geom_defaults("bar",   list(fill   = primary))
}
