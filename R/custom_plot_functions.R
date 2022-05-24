


#' Make a standard horizontal barplot
#' @param data A dataframe
#' @param xvar Name of variable in data to determine bars
#' @param yvar Name of variable in data for length of bars
#' @param palette_function A function that takes a single integer argument to return a vector of colors
#' @param colors If palette_function not provided, a vector of colors (must be at least nrow(data))
#' @param base_size Font base size (title, axes)
#' @param label_size Size of text labels next to bars
#' @param label_k If TRUE, labels are divided by 1000
#' @param label_perc If TRUE, % is shown in label
#' @param bar_width Relative width of bars (1 = fills space completely)
#' @param title Title above plot
#' @param \dots Further arguments passed to `generate_colors`
#' @export
plot_horizontal_bars <- function(data,
                                  xvar = "group",
                                  yvar = "n",

                                  reverse_order = FALSE,
                                  palette_function = NULL,
                                  colors = NULL,
                                  base_size = 14,
                                  label_size = 4,
                                  label_k = FALSE,
                                  label_perc = FALSE,
                                  label_hjust = -0.06,
                                  bar_width = NULL,
                                  title = "", ...){

  data$Y <- data[[yvar]]
  data$group <- data[[xvar]]


  if(reverse_order){
    data$group <- forcats::fct_rev(as.factor(data$group))
  }

  if(nrow(data) == 0){
    return(NULL)
  }

  colors <- generate_colors(nrow(data), palette_function, colors, ...)


  # Om genoeg ruimte te maken voor de bar labels, dit moet algemener (maar hoe...)
  h_multiplier <- 20
  y_max <- max(data$Y, na.rm = TRUE)
  y_axis_max <- y_max / 0.65
  u <- y_max /100 # unit
  y_lim <- c(0, y_axis_max)
  h_adj <- h_multiplier * u


  p <- ggplot(data, aes(x = group, y = Y, fill = group)) +
    geom_bar(stat = "identity", width = bar_width) +
    scale_fill_manual(values = colors, drop = FALSE) +
    coord_flip() +
    theme_minimal() +
    scale_x_discrete(drop=FALSE) +
    theme(  #text = element_text(family = "customplotfont"),
      plot.title = element_text(size=base_size+4),
      panel.border = element_blank(),
      legend.position = "none",
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = base_size),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()) +
    labs(title = title) +
    ylim(y_lim) +
    geom_text(data=data, aes(label = format_n2(Y, label_k, label_perc)),
              hjust = label_hjust, size = label_size)

  p

}




#' Make a standard line plot with time on the X axis
#' @description No group (see plot_grouped_time_plot)
#' @param data A dataframe
#' @param xvar Name of variable in data to determine bars
#' @param yvar Name of variable in data for length of bars
#' @param plot_type Either 'bars' or 'lines'
#' @param palette_function A function that takes a single integer argument to return a vector of colors
#' @param colors If palette_function not provided, a vector of colors (must be at least nrow(data))
#' @param base_size Font base size (title, axes)
#' @param label_size Size of text labels next to bars
#' @param label_k If TRUE, labels are divided by 1000
#' @param label_perc If TRUE, % is shown in label
#' @param bar_width Relative width of bars (1 = fills space completely)
#' @param title Title above plot
#' @param \dots Further arguments passed to `generate_colors`
#' @export
plot_value_by_time <- function(data,
                           xvar = "time",
                           yvar = "n",

                           plot_type = c("bars","lines"),

                           palette_function,
                           colors = NULL,
                           base_size = 14,
                           label_size = 4,
                           point_size = 3,
                           line_width = 1.2,
                           label_bars = FALSE,
                           ylab = "ylab",
                           xlab = "xlab",
                           title = "title", ...
                           ){

  plot_type <- match.arg(plot_type)

  if(nrow(data) == 0){
    return(NULL)
  }

  data$n <- data[[yvar]]
  data$time <- data[[xvar]]

  data <- dplyr::filter(data, !is.na(n), !is.na(time))

  n_time <- length(unique(data$time))

  colors <- generate_colors(1, palette_function, colors, ...)

  if(plot_type == "line"){

    p <- data %>%
      ggplot(aes(x = time, y = n)) +
      geom_point(cex = point_size, colour = colors) +
      geom_line(lwd = line_width, colour = colors) +
      scale_x_continuous(breaks = my_breaks_pretty()) +
      scale_y_continuous(breaks = my_breaks_pretty()) +
      labs(y = ylab, x = xlab, title = title)

  } else {

    p <- data %>%
      ggplot(aes(x=time, y=n)) +
      geom_bar(stat="identity", position = position_stack(),
               fill = colors,
               colour = colors) +
      scale_x_continuous(breaks = my_breaks_pretty()) +
      scale_y_continuous(breaks = my_breaks_pretty()) +
      labs(y = ylab, x = xlab, fill = "", title = title)

    if(label_bars){

      dlab <- data %>%
        group_by(n) %>%
        summarize(n = sum(n), .groups = "drop")

      ymax <- max(dlab$n)

      p <- p + geom_text(data = dlab,
                         aes(label = n, x = time, y = n, fill = NULL, color = NULL),
                         size = label_size,
                         vjust = -1) +
        ylim(c(NA, ymax + 0.05*ymax))

    }

  }

  p <- p +
    theme_minimal(base_size = base_size)
  #theme(text = element_text(family = "customplotfont"))

  p

}






#' Make a standard grouped time plot (lines or stacked bars)
#' @param data A dataframe
#' @param xvar Name of variable in data for X-axis ("time")
#' @param yvar Name of variable for Y-axis
#' @param group Name of variable in data for groups
#' @param plot_type Either 'bars' or 'lines'
#' @param palette_function A function that takes a single integer argument to return a vector of colors
#' @param colors If palette_function not provided, a vector of colors (must be at least nrow(data))
#' @param base_size Font base size (title, axes)
#' @param label_size Size of text labels next to bars
#' @param label_k If TRUE, labels are divided by 1000
#' @param label_perc If TRUE, % is shown in label
#' @param bar_width Relative width of bars (1 = fills space completely)
#' @param title Title above plot
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param grouplab group label (for legend title)
#' @param fill_na_group Value to use for missing group values in data ("Onbekend")
#' @param \dots Further arguments passed to `generate_colors`
#' @export
plot_grouped_value_by_time <- function(data,

                                   xvar = "time",
                                   yvar = "n",
                                   group = "group",

                                   plot_type = c("bars","lines"),

                                   palette_function,
                                   colors = NULL,
                                   base_size = 14,
                                   label_size = 4,
                                   point_size = 3,
                                   line_width = 1.2,
                                   label_bars = FALSE,
                                   ylab = "ylab",
                                   xlab = "xlab",
                                   grouplab = "",
                                   title = "title",
                                   fill_na_group = "Onbekend",
                                   ...
                                   ){

  plot_type <- match.arg(plot_type)

  if(nrow(data) == 0){
    return(NULL)
  }

  data$n <- data[[yvar]]
  data$time <- data[[xvar]]
  data$group <- data[[group]]

  data <- dplyr::filter(data, !is.na(.data$n), !is.na(.data$time))

  if(any(is.na(data$group))){
    data$group <- tidyr::replace_na(data$group, fill_na_group)
  }

  n_time <- length(unique(data$time))
  n_group <- length(unique(data$group))

  colors <- generate_colors(n_group, palette_function, colors, ...)

  if(n_time == 1){

    p <- data %>%
      ggplot(aes(x = group, y = n, fill = group)) +
      geom_bar(stat = "identity") +
      scale_y_continuous(breaks = my_breaks_pretty()) +
      scale_fill_manual(values = colors) +
      theme(axis.text.x=element_blank()) +
      labs(y = xlab, x = "", fill = grouplab, title = title)

  } else {

    if(plot_type == "lines"){

      p <- data %>%
        ggplot(aes(x = time, y = n, color = group)) +
        geom_point(cex = point_size) +
        geom_line(lwd = line_width) +
        scale_x_continuous(breaks = my_breaks_pretty()) +
        scale_y_continuous(breaks = my_breaks_pretty()) +
        scale_colour_manual(values = colors) +
        labs(y = ylab, x = xlab, fill = "", colour = grouplab, title = title)

    } else {

      p <- data %>%
        ggplot(aes(x = time, y = n, fill = group, color = group)) +
        geom_bar(stat="identity", position = position_stack()) +
        scale_x_continuous(breaks = my_breaks_pretty()) +
        scale_y_continuous(breaks = my_breaks_pretty()) +
        scale_colour_manual(values = colors) +
        scale_fill_manual(values = colors) +
        labs(y = ylab, x = xlab, fill = grouplab, title = title) +
        guides(color = "none")

      if(label_bars){

        dlab <- dplyr::group_by(data, time, n) %>%
          dplyr::summarize(n = sum(n), .groups = "drop")

        ymax <- max(dlab$n)

        p <- p + ggplot2::geom_text(data = dlab,
                           aes(label = n, x = time, y = n, fill = NULL, color = NULL),
                           size = label_size,
                           vjust = -1) +
          ggplot2::ylim(c(NA, ymax + 0.05*ymax))

      }

    }

  }


  p <- p +
    theme_minimal(base_size = base_size)
  #theme(text = element_text(family = "customplotfont"))

  p

}







#' Make a standard horizontal barplot with group fill
#' @param data A dataframe made with `table_n_woningen`
#' @param palette_function A function that takes a single integer argument to return a vector of colors
plot_grouped_horizontal_bars <- function(data,
                                            group1 = "group",  # group to make bars
                                            group2 = "group2", # group within bars
                                            yvar = "n",
                                            palette_function,
                                            reverse_palette = FALSE,
                                            colors = NULL,
                                            base_size = 14,
                                            label_size = 4,
                                            label_k = FALSE,
                                            bar_width = NULL,
                                            ylab = "ylab",
                                            title = "",
                                            sort = TRUE,
                                            top_n = NA,
                                            y_multiplier = 1.2){


  data$group1 <- data[[group1]]
  data$group2 <- data[[group2]]
  data$n <- data[[yvar]]

  data <- filter(data, !is.na(group1))

  data$n_total <- ave(data$n, data$group1, FUN = sum)

  if(sort){
    data <- mutate(data, group1 = reorder(group1, n_total, max))
  } else {
    data <- mutate(data, group1 = as.factor(group1))
  }

  data_total <- group_by(data, group1) %>%
    summarize(n = sum(n), .groups = "drop") %>%
    mutate(group1 = factor(group1, levels = levels(data$group1)))

  if(!is.na(top_n)){
    top_n <- as.numeric(top_n)
    n <- nlevels(data$group1)
    top_n <- pmin(top_n,n)

    i_l <- (n-top_n+1):n
    toplevs <- levels(data$group1)[i_l]
    data <- filter(data, group1 %in% toplevs)
    data_total <- filter(data_total, group1 %in% toplevs)

  }


  if(nrow(data) == 0 || sum(data$n) == 0){
    return(NULL)
  }

  g <- levels(data[[group2]])
  colors <- generate_colors(length(g), palette_function, colors, reverse_palette)

  # Om genoeg ruimte te maken voor de bar labels, dit moet algemener (maar hoe...)
  y_max <- max(data$n, na.rm = TRUE)
  y_lim <- c(0, y_max * y_multiplier)

  if(label_k == "auto"){
    label_k <- y_max > 5000
  }


  p <- ggplot(data, aes(x = group1, y = n, fill = group2)) +
    geom_bar(stat = "identity", width = bar_width) +
    scale_fill_manual(values = colors, drop = FALSE) +
    coord_flip(clip = "off") +
    theme_minimal() +
    scale_x_discrete(drop=TRUE) +
    theme(  #text = element_text(family = "customplotfont"),
      plot.title = element_text(size=base_size+4),
      panel.border = element_blank(),
      # legend.position = c(0.4,0.2), #"bottom",
      # legend.justification = "left",
      # legend.direction = "vertical",
      # legend.margin = margin(0),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = base_size),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()) +
    labs(title = title) +
    ylim(y_lim) +
    geom_text(data=data_total, aes(x=group1, label = format_n2(n,label_k,perc=FALSE), fill=NULL),
              hjust = -0.06, size = label_size)

  p

}


