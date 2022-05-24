


#---- Utils ------
table_group_n_woningen <- function(data, column, order = NULL, na_include = TRUE){

  out <- data %>%
    group_by(!!sym(column)) %>%
    summarize(n = sum(aantalwoningen), .groups = "drop") %>%   #! nog hardcoded
    setNames(c("group","n"))

  if(na_include){

    out$group[is.na(out$group)] <- "Onbekend"

    if(!is.null(order) & !any(c("onbekend","Onbekend") %in% order)){
      order <- c("Onbekend", order)
    }

  }

  if(!is.null(order)){

    validate_order(out$group, order)

    mdat <- tibble(x = order) %>% setNames("group")
    out <- left_join(mdat, out, by = "group")
    out$group <- factor(out$group, levels = order)
  }

  out <- mutate(out, n = replace_na(n, 0))

  class(out) <- c(class(out), "table_group_time_n")

  out
}





table_group_time_n_woningen <- function(data, column, time_column, order = NULL, na_include = FALSE){

  out <- data %>%
    group_by(!!sym(column), !!sym(time_column)) %>%
    summarize(n = sum(aantalwoningen), .groups = "drop") %>%   #! nog hardcoded
    setNames(c("group","time","n")) %>%
    group_by(group) %>%
    arrange(time) %>%
    mutate(n_cumu = cumsum(n)) %>%
    ungroup


  if(!is.null(order)){

    validate_order(out$group, order)
    out$group <- factor(out$group, levels = order)

  }

  out <- mutate(out, n = replace_na(n, 0))

out
}


format_table_group_n <- function(x, settings = list(), ...){

  x %>%
    setNames(c(settings$group_label, "Aantal woningen")) %>%
    add_column_percentages("Aantal woningen", "%")

}

format_table_time_n <- function(x, settings = list(), ...){

  x %>%
    setNames(c(settings$time_label, "Aantal woningen", "Aantal woningen totaal"))
}


format_table_group_time_n <- function(x, settings = list(), ...){

  u_nms <- unique(x$group)

  out <- x %>%
    pivot_wider(values_from = n, names_from = group, values_fill = 0)


  names(out)[1:2] <- c(settings$time_label, "Aantal woningen")

  out
    #add_row_percentages("Aantal woningen", "%")

}



validate_order <- function(group, order){
  nms_mis <- setdiff(order, group)
  if(length(nms_mis)>0){
    message(paste("Groups mentioned in customPlot not present in data:", paste(nms_mis,collapse=",")))
  }
  nms_x <- setdiff(group, order)
  if(length(nms_x)>0){
    message(paste("Groups in data not mentioned in customPlot:", paste(nms_x,collapse=",")))
  }
}



validate_plot_settings <- function(settings){

  sett <- names(settings)

  if("custom_function" %in% sett)return(settings)

  template <- settings$template
  stopifnot(!is.null(template))

  fill_settings <- function(settings, default){
    i_mis <- which(!names(default) %in% names(settings))

    if(length(i_mis) > 0){
      settings <- c(settings, default[i_mis])
    }
    settings
  }

  validate_required_settings <- function(settings, required){
    if(!all(required %in% names(settings))){
      stop(glue("Must provide {paste(required,collapse=', ')} in settings for customPlot"))
    }
  }


  if(template == "horizontal_barplot"){

    requi <- c("group","group_label")
    validate_required_settings(settings, requi)

    default <- list(
      group_order = NULL,
      palette_function = "parula",
      colors = NULL,
      filter = NULL,
      label_bars = TRUE,
      base_size = 14,
      label_size = 4,
      label_k = FALSE
    )

  } else if(template == "grouped_time_plot"){

    requi <- c("group","group_label", "time_label", "time_column")
    validate_required_settings(settings, requi)

    default <- list(
      group_order = NULL,
      palette_function = "parula",
      colors = NULL,
      filter = NULL,
      label_bars = FALSE,
      base_size = 14,
      label_size = 4,
      label_k = FALSE,
      point_size = 3,
      line_width = 1.2
    )

  } else if(template == "time_plot"){

    requi <- c("time_label", "time_column")
    validate_required_settings(settings, requi)

    default <- list(
      palette_function = "parula",
      colors = NULL,
      filter = NULL,
      label_bars = FALSE,
      base_size = 14,
      label_size = 4,
      label_k = FALSE,
      point_size = 3,
      line_width = 1.2
    )

  } else NULL

  settings <- fill_settings(settings, default)


  return(settings)
}






#----- UI function ------


customPlotUI <- function(id){

  ns <- NS(id)

  softui::tab_box( style = "margin-top: 10px;",
        softui::tab_panel(title = icon("chart-bar"),
                 plotOutput(ns("plot_main"))
        ),
        softui::tab_panel(title = icon("table"),
                          softui::fluid_row(
                 tags$div(style = "height: 400px;",

                          tags$div(style = "height: 360px; overflow: auto; margin-bottom: 5px",
                                   tableOutput(ns("tab_data"))
                          ),
                          exportButtonUI(ns("btn_download"))
                        )
                  )
        )
    )


}



#----- Server function ------
customPlot <- function(input, output, session,
                       plot_data = reactive(NULL),
                       plot_type = reactive("plot_horizontal_bars"),
                       settings = list(),
                       table_format = function(x)x,
                       extra_ggplot = reactive(NULL),
                       y_min = NULL

                       ){


  internal_plot_types <- c("plot_horizontal_bars","plot_value_by_time","plot_grouped_value_by_time")


  # check settings, fill with default values
  #settings <- validate_plot_settings(settings)
  template <- settings$template


  output$plot_main <- renderPlot({

    if(!is.null(settings$custom_function)){
      plot_fn <- base::get(settings$custom_function$plot)

    } else {

      type <- plot_type()

      if(type %in% internal_plot_types){
        plot_fn <- utils::getFromNamespace(type, "shintoviz")
      } else {
        stop(paste("plot_type not in ",paste(internal_plot_types, collapse= " ,")))
      }
    }

    settings$data <- plot_data()

    # Make the plot using the settings list
    p <- do.call(plot_fn, settings)


    # Extra adjustments
    if(!is.null(y_min)){
      p <- p + expand_limits(y = y_min)
    }

    if(!is.null(extra_ggplot())){

      ex <- extra_ggplot()
      if(!is.list(ex)){
        p <- p + ex
      } else {

        for(i in seq_along(ex)){
          p <- p + ex[[i]]
        }

      }

    }

    p


  })



  #---- Table
  output$tab_data <- renderTable({

      table_format(plot_data())

  } , digits = 0, align = "l")



  # make exportable
  callModule(exportButton, "btn_download", data = plot_data)


}


#table_group_time_n_woningen(wp, "woningtype", order = NULL, time_column = "opleverjaar", na_include = FALSE)

