#' Prepare a table for plotting with various options
#' @description Further adjust a table prepared for plotting: change the order of
#' plotting, sort the levels (by their value), show only the first n values, etc.
#' @param data A dataframe
#' @export
prepare_plot_data <- function(data, groupvar = "group",
                              sort = FALSE,
                              reverse = FALSE,
                              fill_na_group = "Onbekend",
                              top_n = NULL){

  if(any(is.na(data[[groupvar]]))){
    data[[groupvar]] <- tidyr::replace_na(data[[groupvar]], fill_na_group)
  }

  if(sort){
    data[[groupvar]] <- reorder(data[[groupvar]], data$n, max, decreasing = reverse)
  } else {
    data[[groupvar]] <- as.factor(data[[groupvar]])
  }

  if(!is.null(top_n)){
    top_n <- as.numeric(top_n)
    n <- nlevels(data[[groupvar]])
    top_n <- pmin(top_n,n)

    i_l <- (n-top_n+1):n
    toplevs <- levels(data[[groupvar]])[i_l]
    data <- dplyr::filter(data, !!sym(groupvar) %in% toplevs)

  }



return(data)
}
