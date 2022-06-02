#' Set a Google font for plotWidget
#' @description Call this once (typically in global.R)
#' @param googlefont
#' @export
#' @importFrom sysfonts font_add_google
#' @importFrom showtext showtext_auto
#' @examples
#' set_plotwidget_font("Open Sans")
set_plotwidget_font <- function(googlefont){
  sysfonts::font_add_google(googlefont, "customplotfont")
  showtext::showtext_auto()
}
