#' Prepare a table for plotting with various options
#' @description Further adjust a table prepared for plotting: change the order of
#' plotting, sort the levels (by their value), show only the first n values, etc.
#' @param data A dataframe
#' @param yvar Name of the variable to summarize. If groupfun=length, do not provide yvar.
#' @param groupvar Grouping variable in dataframe (quoted)
#' @param groupvar2 A second grouping variable; not yet implemented.
#' @param groupfun A function to apply to the groups (not quoted). If `length`,
#' do not provide a `yvar` argument as the rows will be counted for each group.
#' @param sort If TRUE, sorts the factor levels (and the table) by the group value
#' @param reverse If TRUE, and sort = TRUE, reverses the sort.
#' @param order Optionally a character vector of the desired ordering of the factor levels
#' @param top_n If not NULL, return only the top_n levels (see Example)
#' @param na_include If TRUE, and the grouping variable has missing values, include them in the table
#' @param fill_na_group If `na_include=TRUE`, use this label for missing vaues in the group
#'
#' @export
#' @examples
#' if(require(gapminder)){
#'
#' gap_data <- dplyr::bind_rows(
#'      gapminder,
#'      data.frame(continent = NA_character_, pop = 100e06)
#' )
#' prepare_grouped_data(gap_data,
#'                      groupvar = "continent",
#'                      yvar = "pop",
#'                      groupfun = sum,
#'                      fill_na_group = "Uncharted")
#'
#'
#' prepare_grouped_data(gapminder, groupvar = "continent", groupfun = length)
#'
#' prepare_grouped_data(gapminder, groupvar = "continent", groupfun = length,
#' reverse = TRUE, sort = TRUE)
#'
#'
#' # Top 10 countries by population in 2007
#' gapminder %>%
#'   filter(year == 2007) %>%
#'   prepare_grouped_data(groupvar = "country",
#'                        groupfun = sum,
#'                        yvar = "pop",
#'                        sort = TRUE,
#'                        reverse = TRUE,
#'                        top_n = 10)
#' }
prepare_grouped_data <- function(data,
                              yvar = NULL,
                              groupvar = "group",
                              groupvar2 = NULL,

                              groupfun = NULL,
                              sort = FALSE,
                              reverse = FALSE,

                              order = NULL,
                              top_n = NULL,

                              na_include = TRUE,
                              fill_na_group = "Onbekend"
                              ){


  # Deal with missing group levels
  if(na_include){

    if(any(is.na(data[[groupvar]]))){

      # is already a factor
      if(is.factor(data[[groupvar]])){
        data[[groupvar]] <- forcats::fct_explicit_na(fill_na_group)
      } else {
        data[[groupvar]][is.na(data[[groupvar]])] <- fill_na_group
      }

    }

    if(!is.null(order) & !any(fill_na_group %in% order)){
      order <- c(fill_na_group, order)
    }

  }

  if(is.null(groupfun)){
    stop("Provide a function used to summarize the groups into a single value (e.g. sum, mean, length)")
  }

  # Summarize a variable
  if(!is.null(yvar)){
    data <- dplyr::group_by(data, !!rlang::sym(groupvar)) %>%
      dplyr::summarize(y = groupfun(!!rlang::sym(yvar)), .groups = "drop") %>%
      setNames(c(groupvar,yvar))
  } else {

    # Count rows (no yvar needed)
    data <- dplyr::count(data, !!sym(groupvar)) %>%
      setNames(c(groupvar,"n"))

    yvar <- "n"

  }

  # Sort bars in order of value, or simply make a factor (this will give alphabetic ordering)
  if(sort){
    data[[groupvar]] <- reorder(data[[groupvar]], data[[yvar]], max, decreasing = reverse)
  } else {
    data[[groupvar]] <- as.factor(data[[groupvar]])
  }


  # Order bars based on user config.
  # Using left_join so as to explicitly keep the empty rows
  if(!is.null(order)){

    validate_order(data[[groupvar]], order)
    mdat <- tibble(x = order) %>% setNames(groupvar)
    data <- left_join(mdat, data, by = groupvar)
    data[[groupvar]] <- factor(data[[groupvar]], levels = order)
  }

  # Keep only the top N levels
  if(!is.null(top_n)){
    top_n <- as.numeric(top_n)
    n <- nlevels(data[[groupvar]])
    top_n <- pmin(top_n,n)

    i_l <- 1:top_n
    toplevs <- levels(data[[groupvar]])[i_l]
    data <- droplevels(dplyr::filter(data, !!sym(groupvar) %in% toplevs))

  }

  # Sort table to the levels of the group factor.
  # Has no effect on e.g. ggplot2 because there the factor levels are used.
  # It is useful to have the table in the same order as the factor levels though.
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


