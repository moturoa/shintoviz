
#' Generate vector of colors
#' @param n Number of colors
#' @param palette_function Function to generate colors (e.g. "parula")
#' @param colors Vector of colors (must be length >= n)
#' @param reverse_palette If TRUE, reverse colors
#' @export
generate_colors <- function(n, palette_function = NULL, colors = NULL, reverse_palette = FALSE){

  if(!is.null(colors)){

    stopifnot(length(colors) >= n)
    out <- colors[1:n]

  } else {

    if(is.null(palette_function)){
      stop("Provide a palette_function (name of function in the pals package) or vector of colors")
    }
    palfun <- base::get(palette_function)
    out <- palfun(n)
  }

  if(reverse_palette){
    out <- rev(out)
  }


  out
}

