#' Set a Google font for plotWidget
#' @description Call this once (typically in global.R)
#' @param googlefont
#' @export
#' @importFrom sysfonts font_add_google
#' @importFrom showtext showtext_auto
#' @importFrom sysfonts font_families
#' @examples
#' set_plotwidget_font("Open Sans")
set_plotwidget_font <- function(googlefont){
  sysfonts::font_add_google(googlefont, "customplotfont")
  showtext::showtext_auto()
}


# Util om de font family te kiezen. Normaliter customplotfont tenzij
# deze niet is ingesteld.
get_current_font_family <- function(){

  if("customplotfont" %in% sysfonts::font_families()){
    "customplotfont"
  } else {
    "sans"
  }
}


