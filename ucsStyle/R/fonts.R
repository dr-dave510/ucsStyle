#' Register Gotham from the Windows AppData fonts folder
#' @export
ucs_register_gotham <- function(family = "Gotham", enable_showtext = TRUE) {
  if (.Platform$OS.type != "windows") {
    stop("ucs_register_gotham() currently targets Windows per-user font folder.")
  }
  local_fonts_dir <- file.path(Sys.getenv("LocalAppData"), "Microsoft", "Windows", "Fonts")
  if (!dir.exists(local_fonts_dir)) {
    stop("User fonts folder not found: ", local_fonts_dir)
  }
  gotham_regular <- Sys.glob(file.path(local_fonts_dir, "*Gotham*Book*.ttf"))
  if (length(gotham_regular) == 0) gotham_regular <- Sys.glob(file.path(local_fonts_dir, "*Gotham*Regular*.ttf"))
  if (length(gotham_regular) == 0) gotham_regular <- Sys.glob(file.path(local_fonts_dir, "Gotham*.ttf"))
  gotham_regular <- gotham_regular[1]
  gotham_bold   <- Sys.glob(file.path(local_fonts_dir, "*Gotham*Bold*.ttf"))[1]
  gotham_italic <- Sys.glob(file.path(local_fonts_dir, "*Gotham*Italic*.ttf"))[1]
  if (is.na(gotham_regular) || !file.exists(gotham_regular)) {
    stop("Could not locate a Gotham regular/Book/Regular TTF in: ", local_fonts_dir)
  }
  sysfonts::font_add(
    family  = family,
    regular = gotham_regular,
    bold    = if (!is.na(gotham_bold)) gotham_bold else NULL,
    italic  = if (!is.na(gotham_italic)) gotham_italic else NULL
  )
  if (isTRUE(enable_showtext)) showtext::showtext_auto()
  invisible(list(regular = gotham_regular, bold = gotham_bold, italic = gotham_italic))
}
