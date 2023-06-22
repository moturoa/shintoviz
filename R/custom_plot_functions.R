


#' Make a standard horizontal barplot
#' @param data A dataframe
#' @param xvar Name of variable in data to determine bars
#' @param yvar Name of variable in data for length of bars
#' @param palette_function A function that takes a single integer argument to return a vector of colors
#' @param colors If palette_function not provided, a vector of colors (must be at least nrow(data))
#' @param base_size Font base size (title, axes)
#' @param label_size Size of text labels next to bars
#' @param label_k If TRUE, labels are divided by 1000
#' @param label_perc If TRUE, percentage is shown in label
#' @param bar_width Relative width of bars (1 = fills space completely)
#' @param reverse_order Plot order of bars opposite to factor levels
#' @param label_hjust Space between bar label and bar (negative value = outside bar as the default)
#' @param title Title above plot
#' @param \dots Further arguments passed to `generate_colors`
#' @export
#' @examples
#' library(gapminder)
#' library(dplyr)
#'
#' plot_data <- gapminder %>%
#'   filter(year == 2007) %>%
#'   group_by(continent) %>%
#'   summarize(population = floor(1e-06 * sum(pop)), .groups = "drop")
#'
#' plot_horizontal_bars(plot_data,
#'                      xvar = "continent",
#'                      yvar = "population",
#'                      palette_function = "ocean.phase",
#'                      colors = NULL,
#'                      base_size = 14,
#'                      label_size = 4,
#'                      label_k = FALSE,
#'                      bar_width = 0.62,
#'                      title = "Populatie (miljoenen)")
#' @importFrom forcats fct_rev
#' @importFrom ggplot2 ggplot aes geom_bar geom_line geom_point scale_fill_manual
#' @importFrom ggplot2 theme scale_x_discrete scale_y_continuous position_stack position_dodge
#' @importFrom ggplot2 geom_text ylim labs theme_minimal coord_flip element_text element_blank
#' @importFrom grid gpar
plot_horizontal_bars <- function(data,
                                  xvar = "group",
                                  yvar = "n",

                                  group_format_function = NULL,
                                  reverse_order = FALSE,
                                  palette_function = NULL,
                                  colors = NULL,
                                  base_size = 15,
                                  label_function = NULL,
                                  label_size = 5,
                                  label_k = FALSE,
                                  label_perc = FALSE,
                                  label_hjust = -0.2,
                                  bar_width = 0.6,
                                  title = "",
                                  subtitle = "",
                                  title_adjust = c("plot","figure"),
                                  ...){

  title_adjust <- match.arg(title_adjust)
  font_family <- get_current_font_family()

  data$Y <- data[[yvar]]
  data$Y[is.na(data$Y)] <- 0

  data$group <- data[[xvar]]

  data$label <- make_value_label(values = data$Y, label_function = label_function,
                                 label_k = label_k,
                                 label_perc = label_perc)

  if(reverse_order){
    data$group <- forcats::fct_rev(as.factor(data$group))
  }


  if(!is.null(group_format_function)){
    group_format_function <- base::get(group_format_function)
    levs <- group_format_function(levels(data$group))
    levels(data$group) <- levs
  }

  # 0 of 1 rijen geen nuttige plot
  if(nrow(data) < 2){
    p <- plot_not_sufficient_data()
    return(p)
  }

  # TODO palette reverse? shintomap kan dat wel
  ncols <- nrow(data)
  if(is.factor(data$group)){
    ncols <- nlevels(data$group)
  }
  colors <- generate_colors(ncols, palette_function, colors)

  p <- ggplot2::ggplot(data, aes(x = Y, y = group, fill = group)) +
    ggplot2::geom_col(width = bar_width) +
    ggplot2::scale_fill_manual(values = colors, drop = FALSE) +

    ggplot2::theme_minimal() +
    ggplot2::scale_y_discrete(drop=FALSE) +  # do not drop empty bars

    ggplot2::theme(  text = ggplot2::element_text(family = font_family),
      plot.title = ggplot2::element_text(size=base_size+4),
      panel.border = ggplot2::element_blank(),
      legend.position = "none",
      axis.title.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = base_size),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::labs(title = title, subtitle = subtitle) +
    ggplot2::geom_text(data=data, ggplot2::aes(label = label),
              family = font_family,
              hjust = label_hjust, size = label_size) +
    expand_limits(x=0)

  if(title_adjust == "plot"){
    p <- p + ggplot2::theme(
      plot.caption = element_text(hjust = 0),
      plot.title.position = "plot"
    )
  }

  p <- fit_horizontal_bar_labels({p})

  p

}




#' Make a standard line plot with time on the X axis
#' @description No group (see plot_grouped_time_plot)
#' @param data A dataframe
#' @param xvar Name of variable in data to determine bars
#' @param yvar Name of variable in data for length of bars
#' @param sub_type Either 'bars' or 'lines'
#' @param palette_function A function that takes a single integer argument to return a vector of colors
#' @param colors If palette_function not provided, a vector of colors (must be at least nrow(data))
#' @param base_size Font base size (title, axes)
#' @param label_size Size of text labels next to bars
#' @param label_k If TRUE, labels are divided by 1000
#' @param label_perc If TRUE, percentage is shown in label
#' @param point_size Size of points plotted on line
#' @param line_width Line width if sub_type = "lines"
#' @param bar_width Relative width of bars (1 = fills space completely)
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param title Title above plot
#' @param \dots Further arguments passed to `generate_colors`
#' @export
#' @examples
#' library(gapminder)
#' library(dplyr)
#' plot_data <- gapminder %>%
#'   filter(country == "Netherlands") %>%
#'   mutate(pop = 1e-06* pop)
#'
#' plot_value_by_time(plot_data,
#'                    xvar = "year",
#'                    yvar = "pop",
#'                    sub_type = "bars",
#'                    palette_function = NULL,
#'                    colors = "red",
#'                    base_size = 14,
#'                    label_size = 4,
#'                    point_size = 3,
#'                    line_width = 1.2,
#'                    label_bars = FALSE,
#'                    ylab = "Populatie",
#'                    xlab = "Jaar",
#'                    title = "Nederland")
plot_value_by_time <- function(data,
                           xvar = "time",
                           yvar = "n",

                           sub_type = c("bars","lines"),

                           palette_function,
                           colors = NULL,
                           base_size = 14,
                           label_size = 4,
                           point_size = 3,
                           line_width = 1.2,
                           bar_width = 0.6,
                           label_function = NULL,
                           label_bars = FALSE,
                           label_k = FALSE,
                           label_perc = FALSE,
                           ylab = "ylab",
                           xlab = "xlab",
                           title = "",
                           subtitle = "",
                           ...
                           ){

  sub_type <- match.arg(sub_type)
  font_family <- get_current_font_family()

  if(nrow(data) == 0){
    return(NULL)
  }

  data$n <- data[[yvar]]
  data$time <- data[[xvar]]

  # TODO dit is voor de situatie dat de boel uit prepare_grouped_data komt,
  # met time als factor. dat kan dus niet..
  if(!inherits(data$time, "Date")){
    data$time <- as.integer(as.character(data$time))
  }

  data <- dplyr::filter(data, !is.na(n), !is.na(time))

  n_time <- length(unique(data$time))

  colors <- generate_colors(1, palette_function, colors)

  if(sub_type == "lines"){

    p <- data %>%
      ggplot2::ggplot(ggplot2::aes(x = time, y = n)) +
      ggplot2::geom_point(cex = point_size, colour = colors) +
      ggplot2::geom_line(linewidth = line_width, colour = colors) +
      ggplot2::labs(y = ylab, x = xlab, title = title)

  } else {

    p <- data %>%
      ggplot2::ggplot(aes(x=time, y=n)) +
      ggplot2::geom_bar(stat="identity", position = ggplot2::position_stack(),
               fill = colors, width = bar_width,
               colour = colors) +
      ggplot2::labs(y = ylab, x = xlab, fill = "", title = title,
                    subtitle = subtitle)

  }

  # X axis
  if(!inherits(data$time, "Date")){
    p <- p +
      ggplot2::scale_x_continuous(breaks = my_breaks_pretty())
  } else {
    # some duplication here; might change the defaults for the x axis
    p <- p +
      scale_x_date(labels = date_format("%d/%m/'%y"))

  }

  # Y axis
  p <- p + ggplot2::scale_y_continuous(breaks = my_breaks_pretty())

  if(label_bars){

    data$label <- make_value_label(values = data$n,
                                   label_function = label_function,
                                   label_k = label_k,
                                   label_perc = label_perc)

    ymax <- max(data$n)
    p <- p + ggplot2::geom_text(data = data,
                                ggplot2::aes(label = label, x = time, y = n, fill = NULL, color = NULL),
                                size = label_size,
                                family = font_family,
                                vjust = -1) +
      ggplot2::ylim(c(NA, ymax + 0.05*ymax))

  }

  p <- p +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(text = ggplot2::element_text(family = font_family))

  p

}






#' Make a standard grouped time plot (lines or stacked bars)
#' @param data A dataframe
#' @param xvar Name of variable in data for X-axis ("time")
#' @param yvar Name of variable for Y-axis
#' @param group Name of variable in data for groups
#' @param sub_type Either 'bars' or 'lines'
#' @param palette_function A function that takes a single integer argument to return a vector of colors
#' @param colors If palette_function not provided, a vector of colors (must be at least nrow(data))
#' @param base_size Font base size (title, axes)
#' @param label_size Size of text labels next to bars
#' @param point_size Size of points plotted on line
#' @param line_width Width of line (if sub_type = "lines")
#' @param label_bars TRUE/FALSE, label the bars or not.
#' @param label_k If TRUE, labels are divided by 1000
#' @param label_perc If TRUE, % is shown in label
#' @param bar_width Relative width of bars (1 = fills space completely)
#' @param title Title above plot
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param grouplab group label (for legend title)
#' @param \dots Further arguments passed to `generate_colors`
#' @export
#' @importFrom ggplot2 guides
#' @examples
#' library(gapminder)
#' library(dplyr)
#' plot_data <- gapminder %>%
#'   filter(country %in% c("Netherlands","Belgium","Luxembourg","Denmark")) %>%
#'   mutate(pop = 1e-06* pop)
#'
#' plot_grouped_value_by_time(plot_data,
#'                            xvar = "year",
#'                            yvar = "pop",
#'                            group = "country",
#'                            sub_type = "lines",
#'                            palette_function = "viridis",
#'                            colors = NULL,
#'                            base_size = 14,
#'                            label_size = 4,
#'                            point_size = 3,
#'                            line_width = 1.2,
#'                            label_bars = FALSE,
#'                            ylab = "Populatie",
#'                            xlab = "Jaar",
#'                            title = "West-Europa") +
#'   ylim(c(0,20))
plot_grouped_value_by_time <- function(data,

                                   xvar = "time",
                                   yvar = "n",
                                   group = "group",

                                   sub_type = c("stacked_bars","grouped_bars","lines"),

                                   palette_function,
                                   colors = NULL,
                                   base_size = 14,
                                   label_size = 4,
                                   point_size = 3,
                                   line_width = 1.2,
                                   bar_width = 0.6,
                                   label_function = NULL,
                                   label_bars = FALSE,
                                   label_k = FALSE,
                                   label_perc = FALSE,
                                   legend.position = "right",
                                   ylab = "ylab",
                                   xlab = "xlab",
                                   grouplab = "",
                                   title = "",
                                   subtitle = "",
                                   ...
                                   ){

  sub_type <- match.arg(sub_type)
  font_family <- get_current_font_family()

  if(nrow(data) == 0){
    return(NULL)
  }

  data$n <- data[[yvar]]
  data$time <- data[[xvar]]
  data$group <- data[[group]]

  data <- dplyr::filter(data, !is.na(.data$n), !is.na(.data$time))

  n_time <- length(unique(data$time))
  n_group <- length(unique(data$group))

  colors <- generate_colors(n_group, palette_function, colors)

  if(n_time == 1){

      p <- data %>%
        ggplot2::ggplot(ggplot2::aes(x = group, y = n, fill = group)) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::scale_y_continuous(breaks = my_breaks_pretty()) +
        ggplot2::scale_fill_manual(values = colors) +
        ggplot2::theme(axis.text.x=element_blank()) +
        ggplot2::labs(y = xlab, x = "", fill = grouplab, title = title)

  } else {

    if(sub_type == "lines"){

      p <- data %>%
        ggplot2::ggplot(aes(x = time, y = n, color = group)) +
        ggplot2::geom_point(cex = point_size) +
        ggplot2::geom_line(linewidth = line_width) +
        ggplot2::scale_x_continuous(breaks = my_breaks_pretty()) +
        ggplot2::scale_y_continuous(breaks = my_breaks_pretty()) +
        ggplot2::scale_colour_manual(values = colors) +
        ggplot2::labs(y = ylab, x = xlab, fill = "", colour = grouplab, title = title, subtitle = subtitle)

    } else {

      pos <- if(sub_type == "grouped_bars"){
        ggplot2::position_dodge()
      } else {
        ggplot2::position_stack()
      }

      p <- data %>%
        ggplot2::ggplot(aes(x = time, y = n, fill = group, color = group)) +
        ggplot2::geom_bar(stat="identity", position = pos, width = bar_width) +
        ggplot2::scale_x_continuous(breaks = my_breaks_pretty()) +
        ggplot2::scale_y_continuous(breaks = my_breaks_pretty()) +
        ggplot2::scale_colour_manual(values = colors) +
        ggplot2::scale_fill_manual(values = colors) +
        ggplot2::labs(y = ylab, x = xlab, fill = grouplab, title = title, subtitle = subtitle) +
        ggplot2::guides(color = "none")

    }

  }

  if(label_bars){


    if(sub_type == "stacked_bars"){
      # Stacked bars: label only the total (sum)
      label_data <- dplyr::group_by(data, time) %>%
        dplyr::summarize(n = sum(n), .groups = "drop")
    } else {
      # Else label everything
      label_data <- data
    }

    label_data$label <- make_value_label(values = label_data$n,
                                   label_function = label_function,
                                   label_k = label_k,
                                   label_perc = label_perc)

    ymax <- max(label_data$n)

    suppressWarnings({
      p <- p + ggplot2::geom_text(data = label_data,
                                  ggplot2::aes(label = n, x = time, y = n, fill = NULL, color = NULL),
                                  size = label_size,
                                  family = font_family,
                                  vjust = -1) +
        ggplot2::ylim(c(NA, ymax + 0.05*ymax))
    })

  }


  p <- p +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(text = element_text(family = font_family),
                   legend.position = legend.position)

  p

}



#' Make a standard piechart
#' @param data A dataframe
#' @param xvar Name of variable in data for X-axis ("time")
#' @param yvar Name of variable for Y-axis
#' @param group Name of variable in data for groups
#' @param sub_type Either 'bars' or 'lines'
#' @param palette_function A function that takes a single integer argument to return a vector of colors
#' @param colors If palette_function not provided, a vector of colors (must be at least nrow(data))
#' @param base_size Font base size (title, axes)
#' @param label_size Size of text labels next to bars
#' @param point_size Size of points plotted on line
#' @param line_width Width of line (if sub_type = "lines")
#' @param label_bars TRUE/FALSE, label the bars or not.
#' @param label_k If TRUE, labels are divided by 1000
#' @param label_perc If TRUE, % is shown in label
#' @param bar_width Relative width of bars (1 = fills space completely)
#' @param title Title above plot
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param grouplab group label (for legend title)
#' @param \dots Further arguments passed to `generate_colors`
#' @export
#' @importFrom ggplot2 guides
#'
plot_pie_chart <- function(data,
                           xvar = "group",
                           yvar = "n",

                           palette_function,
                           colors = NULL,
                           base_size = 14,

                           legend.position = "left",

                           label_function = NULL,
                           label_size = 4,
                           label_hjust = 1.65,
                           title = "title",
                           ... ){



  if(nrow(data) == 0){
    return(NULL)
  }

  font_family <- get_current_font_family()

  n_group <- length(unique(data[[xvar]]))

  colors <- generate_colors(n_group, palette_function, colors)

  totalY <- sum(data[[yvar]])
  data$yvar_perc <- data[[yvar]] / totalY * 100

  data$group <- data[[xvar]]
  data$label <- paste0(make_value_label(values = data$yvar_perc,
                                 label_function = label_function,
                                 label_perc = FALSE), "%")


  # Basic piechart
  p <- ggplot(data, aes(x="", y = yvar_perc, fill = group)) +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0) +
    scale_fill_manual(values = colors, name = "") +
    theme_void() +
    ggplot2::labs(x = "", y = "",title = title) +
    geom_text(aes(x = label_hjust, label = label),
              position = position_stack(vjust=0.5), size = label_size,
              family = font_family) +
    theme(text = ggplot2::element_text(family = font_family),
          legend.position = legend.position,
          legend.text = element_text(size=base_size+2),
          legend.title = element_blank(),
          plot.title = ggplot2::element_text(size=base_size+4),
          plot.caption = element_text(hjust = 0.5))
  p
}



#' Make a standard horizontal barplot with group fill
#' @export
plot_grouped_horizontal_barplot <- function(data,
                                         xvar = "group",
                                         yvar = "n",
                                         fillvar = xvar,

                                         group_format_function = NULL,
                                         sort = TRUE,
                                         top_n = NA,
                                         reverse_order = FALSE,

                                         palette_function = NULL,
                                         reverse_palette = FALSE,
                                         colors = NULL,
                                         base_size = 15,
                                         label_function = NULL,
                                         label_size = 5,
                                         label_k = FALSE,
                                         label_perc = FALSE,
                                         label_hjust = -0.2,
                                         bar_width = 0.6,
                                         title = "",
                                         subtitle = "",
                                         title_adjust = c("plot","figure"),
                                         ...){

  title_adjust <- match.arg(title_adjust)
  font_family <- get_current_font_family()

  data$Y <- data[[yvar]]
  data$Y[is.na(data$Y)] <- 0

  data$group <- data[[xvar]]
  data$fillvar <- data[[fillvar]]

  # Totals. For label placement.
  data$n_total <- ave(data$Y, data$group, FUN = sum)

  if(sort){
    data <- mutate(data, group = reorder(group, n_total, max))
  }

  # Eerste level moet links staan, niet rechts
  data$fillvar <- forcats::fct_rev(data$fillvar)


  if(reverse_order){
    data$group <- forcats::fct_rev(as.factor(data$group))
  }

  data_total <- group_by(data, group) %>%
    summarize(Y = sum(Y), .groups = "drop") %>%
    mutate(group = factor(group, levels = levels(data$group)))

  if(!is.na(top_n)){
    top_n <- as.numeric(top_n)
    n <- nlevels(data$group)
    top_n <- pmin(top_n,n)

    i_l <- (n-top_n+1):n
    toplevs <- levels(data$group)[i_l]
    data <- filter(data, group %in% toplevs) %>% droplevels
    data_total <- filter(data_total, group %in% toplevs) %>% droplevels

  }

  data_total$label <- make_value_label(values = data_total$Y,
                                       label_function = label_function,
                                       label_k = label_k,
                                       label_perc = label_perc)



  if(!is.null(group_format_function)){
    group_format_function <- base::get(group_format_function)
    levs <- group_format_function(levels(data$group))
    levels(data$group) <- levs
  }

  # 0 of 1 rijen geen nuttige plot
  if(nrow(data) < 2){
    p <- plot_not_sufficient_data()
    return(p)
  }

  # Generate fill colors
  ncols <- nrow(data)

  if(is.factor(data$fillvar)){
    ncols <- nlevels(data$fillvar)
  }
  colors <- generate_colors(ncols, palette_function, colors, reverse_palette = reverse_palette)

  # Make plot
  p <- ggplot2::ggplot(data, aes(x = Y, y = group, fill = fillvar)) +
    ggplot2::geom_col(width = bar_width) +
    ggplot2::scale_fill_manual(values = colors, drop = FALSE, guide = guide_legend(reverse = TRUE)) +

    ggplot2::theme_minimal()  +
    ggplot2::scale_y_discrete(drop=FALSE) + # do not drop empty bars

    ggplot2::theme(  text = ggplot2::element_text(family = font_family),
                     plot.title = ggplot2::element_text(size=base_size+4),
                     panel.border = ggplot2::element_blank(),
                     legend.position = "top",
                     legend.direction = "horizontal",
                     legend.title = element_blank(),
                     axis.title.y = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_text(size = base_size),
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::labs(title = title, subtitle = subtitle) +

    ggplot2::geom_text(data = data_total,
                       ggplot2::aes(y = group, label = label, fill = NULL),
                       family = font_family,
                       hjust = label_hjust, size = label_size)
  expand_limits(x=0)

  if(title_adjust == "plot"){
    p <- p + ggplot2::theme(
      plot.caption = element_text(hjust = 0),
      plot.title.position = "plot"
    )
  }

  p <- fit_horizontal_bar_labels({p})

  p

}





plot_distribution_by_group <- function(data,
                                       xvar = "",
                                       group = "",
                                       base_size = 15,
                                       ylab = "",
                                       xlab = "",
                                       n_bins = 20){



  data$X <- data[[xvar]]
  data$group <- data[[group]]

  ggplot(data, aes(x = X, fill = group)) +
    geom_histogram(bins = n_bins) +
    theme_minimal(base_size = base_size) +
    facet_wrap(~group, nrow = 2, ncol = 1) +
    ylab(ylab) + xlab(xlab)


}




