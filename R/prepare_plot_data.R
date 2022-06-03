#' Prepare a table for plotting with various options
#' @description Further adjust a table prepared for plotting: change the order of
#' plotting, sort the levels (by their value), show only the first n values, etc.
#' @param data A dataframe
#' @export
prepare_grouped_data <- function(data,
                              groupvar = "group",
                              yvar = NULL,

                              groupfun = NULL,
                              sort = FALSE,
                              reverse = FALSE,

                              order = NULL,

                              fill_na_group = "Onbekend",
                              top_n = NULL){

  # TODO
  # if(na_include){
  #
  #   out$group[is.na(out$group)] <- "Onbekend"
  #
  #   if(!is.null(order) & !any(c("onbekend","Onbekend") %in% order)){
  #     order <- c("Onbekend", order)
  #   }
  #
  # }

  stopifnot(!is.null(yvar))

  # NA levels - fill.
  if(any(is.na(data[[groupvar]]))){
    data[[groupvar]] <- tidyr::replace_na(data[[groupvar]], fill_na_group)
  }

  if(!is.null(yvar)){
    data <- dplyr::group_by(data, !!sym(groupvar)) %>%
      summarize(y = groupfun(!!sym(yvar)), .groups = "drop") %>%
      setNames(c(groupvar,yvar))
  }

  # Sort bars in order of value
  if(sort){
    data[[groupvar]] <- reorder(data[[groupvar]], data[[yvar]], max, decreasing = reverse)
  } else {
    data[[groupvar]] <- as.factor(data[[groupvar]])
  }


  # Order bars based on user config
  if(!is.null(order)){

    validate_order(data[[groupvar]], order)
    mdat <- tibble(x = order) %>% setNames(groupvar)
    data <- left_join(mdat, data, by = groupvar)
    data[[groupvar]] <- factor(data[[groupvar]], levels = order)
  }


  # TODO niet algemeen
  if(!is.null(top_n)){
    top_n <- as.numeric(top_n)
    n <- nlevels(data[[groupvar]])
    top_n <- pmin(top_n,n)

    i_l <- (n-top_n+1):n
    toplevs <- levels(data[[groupvar]])[i_l]
    data <- droplevels(dplyr::filter(data, !!sym(groupvar) %in% toplevs))

  }

  # Sort tabel to the levels of the group factor. For ggplot2 not necessary but
  # for the table data it is.
  data <- arrange(data, !!sym(groupvar))


return(data)
}





# TODO wordt nog niet gebruikt
validate_order <- function(group, order){
  nms_mis <- setdiff(order, group)
  if(length(nms_mis)>0){
    message(paste("Groups mentioned in plotWidget not present in data:", paste(nms_mis,collapse=",")))
  }
  nms_x <- setdiff(group, order)
  if(length(nms_x)>0){
    message(paste("Groups in data not mentioned in plotWidget:", paste(nms_x,collapse=",")))
  }
}


