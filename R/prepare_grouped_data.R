#' Prepare a table for plotting with various options
#' @description Further adjust a table prepared for plotting: change the order of
#' plotting, sort the levels (by their value), show only the first n values, etc.
#' @param data A dataframe
#' @param yvar Name of the variable to summarize. If groupfun=length, do not provide yvar.
#' @param groupvar Grouping variable in dataframe (quoted)
#' @param groupfun A function to apply to the groups (not quoted). If `length`,
#' do not provide a `yvar` argument as the rows will be counted for each group.
#' @param sort If TRUE, sorts the factor levels (and the table) by the group value
#' @param reverse If TRUE, and sort = TRUE, reverses the sort.
#' @param order Optionally a character vector of the desired ordering of the factor levels
#' @param top_n If not NULL, return only the top_n levels (see Example)
#' @param na_include If TRUE, and the grouping variable has missing values, include them in the table
#' @param fill_na_group If `na_include=TRUE`, use this label for missing vaues in the group
#' @importFrom rlang sym
#' @importFrom forcats fct_na_value_to_level
#' @importFrom tibble tibble
#' @importFrom dplyr arrange count group_by summarize
#' @importFrom stats setNames reorder
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
                                 groupfun = NULL,
                                 groupvar2 = NULL,
                                 sort = FALSE,
                                 reverse = TRUE,
                                 order = NULL,
                                 top_n = NULL,
                                 na_include = TRUE,
                                 fill_na_group = "Onbekend",
                                 empty_char_to_na = TRUE,
                                 array = FALSE,
                                 filter = NULL,
                                 array_encoding = c("semicolon","json")){



  if(!groupvar %in% names(data)){
    message(glue::glue("groupvar '{groupvar}' not found in data"))
    return(NULL)
  }

  if(!is.null(groupvar2) && !groupvar %in% names(data)){
    message(glue::glue("groupvar2 '{groupvar2}' not found in data"))
    return(NULL)
  }


  if(!is.null(filter)){
    data <- dplyr::filter(data, eval(parse(text=filter)))
  }

  # Deal with missing group levels
  if(empty_char_to_na){
    if(is.character(data[[groupvar]])){
      data[[groupvar]] <- dplyr::na_if(data[[groupvar]], "")
    } else {
      if(any(is.na(data[[groupvar]]))){
        warning("shintoviz prepare_grouped_data: don't know how to fill missing groups when integer, fix yourself!")
      }
    }
  }

  if(na_include){

    if(any(is.na(data[[groupvar]]))){

      # is already a factor
      if(is.factor(data[[groupvar]])){
        data[[groupvar]] <- forcats::fct_na_value_to_level(data[[groupvar]], fill_na_group)
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

  if(is.character(groupfun)){
    groupfun <- base::get(groupfun)
  }

  # Summarize a variable
  if(!is.null(yvar)){

    if(!yvar %in% names(data)){
      message(glue::glue("yvar '{yvar}' not found in data"))
      return(NULL)
    }

    if(array)warning("'array' argument ignored when yvar is not NULL (array is only meaningful to count observations)")

    data <- dplyr::group_by(data, !!!rlang::syms(c(groupvar,groupvar2))) %>%
      dplyr::summarize(y = groupfun(!!rlang::sym(yvar)), .groups = "drop")

    if(is.null(groupvar2)){
      data <- stats::setNames(data, c(groupvar,yvar))
    } else {
      data <- stats::setNames(data, c(groupvar,groupvar2,yvar))
    }

  } else {

    if(!array){
      # Count rows (no yvar needed)
      data <- dplyr::count(data, !!!rlang::syms(c(groupvar,groupvar2)))

      if(is.null(groupvar2)){
        data <- stats::setNames(data, c(groupvar, "n"))
      } else {
        data <- stats::setNames(data, c(groupvar,groupvar2, "n"))
      }

    } else {

      if(!is.null(groupvar2)){
        message("Argment 'groupvar2' ignored when array = TRUE - not yet possible")
      }

      array_encoding <- match.arg(array_encoding)
      if(array_encoding == "semicolon"){
        values <- strsplit(data[[groupvar]], ";")
      } else {
        values <- from_json(data[[groupvar]])
      }

      if(is.list(values)){
        values <- do.call(c, values)
      }

      # rare bug: als letterlijk "null" in de DB staat wordt het door jsonlite NULL
      if(is.null(values)){
        values <- fill_na_group
      }

      data <- count(data.frame(values = values), values) %>%
        stats::setNames(c(groupvar,"n"))

    }


    yvar <- "n"

  }

  # Sort bars in order of value, or simply make a factor (this will give alphabetic ordering)
  if(sort){
    data[[groupvar]] <- stats::reorder(data[[groupvar]], data[[yvar]], max, decreasing = reverse)
  } else {
    data[[groupvar]] <- as.factor(data[[groupvar]])
  }


  # Order bars based on user config.
  # Using left_join so as to explicitly keep the empty rows
  if(!is.null(order)){

    validate_order(data[[groupvar]], order)
    mdat <- tibble::tibble(x = order) %>% stats::setNames(groupvar)
    data <- dplyr::left_join(mdat, data, by = groupvar)
    data[[groupvar]] <- factor(data[[groupvar]], levels = order)
  }

  # Keep only the top N levels
  if(!is.null(top_n)){
    top_n <- as.numeric(top_n)
    n <- nlevels(data[[groupvar]])
    top_n <- pmin(top_n,n)

    i_l <- 1:top_n
    toplevs <- levels(data[[groupvar]])[i_l]
    data <- droplevels(dplyr::filter(data, !!rlang::sym(groupvar) %in% toplevs))

  }

  # Sort table to the levels of the group factor.
  # Has no effect on e.g. ggplot2 because there the factor levels are used.
  # It is useful to have the table in the same order as the factor levels though.
  data <- dplyr::arrange(data, !!rlang::sym(groupvar))


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


