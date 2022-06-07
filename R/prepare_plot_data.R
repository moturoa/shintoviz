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
                              top_n = NULL,

                              na_include = TRUE,
                              fill_na_group = "Onbekend"
                              ){


  if(na_include){

    data[[groupvar]][is.na(data[[groupvar]])] <- fill_na_group

    if(!is.null(order) & !any(fill_na_group %in% order)){
      order <- c(fill_na_group, order)
    }

  }

  if(is.null(groupfun)){
    stop("Provide a function used to summarize the groups into a single value (e.g. sum, mean, length)")
  }

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



#----- Utils (not exported)

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


