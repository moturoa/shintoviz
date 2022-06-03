


#---- Utils ------


# TODO niet gebruikt maar de ordering en na_include moeten naar prepare_plot_data
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










# TODO nog niet aangepast sinds WBM3.0
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

#' Shiny UI function for plotWidget
#' @param id Shiny input ID
#' @param header_ui Further UI to be placed above the plot
#' @param footer_ui Further UI to be placed below the plot
#' @param \dots Further arguments to softui::tab_box
#' @rdname plotWidget
#' @export
plotWidgetUI <- function(id, header_ui = NULL, footer_ui = NULL, ...){

  ns <- NS(id)

  softui::tab_box( style = "margin-top: 10px;", ...,
        softui::tab_panel(title = bsicon("bar-chart-fill"),
                          header_ui,
                          plotOutput(ns("plot_main")),
                          footer_ui
        ),
        softui::tab_panel(title = bsicon("table"),
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

#' @rdname plotWidget
#' @export
plotWidgetModule <- function(input, output, session,
                       plot_data = reactive(NULL),
                       plot_type = reactive("plot_horizontal_bars"),
                       settings = reactive(list()),
                       table_format = function(x)x,
                       extra_ggplot = reactive(NULL),
                       y_min = NULL
                       ){


  output$plot_main <- shiny::renderPlot({

    sett <- settings()

    # check settings, fill with default values
    #settings <- validate_plot_settings(settings)
    template <- sett$template


    if(!is.null(sett$custom_function)){
      plot_fn <- base::get(sett$custom_function$plot)

    } else {

      type <- plot_type()

      if(type %in% internal_custom_plot_types){
        plot_fn <- utils::getFromNamespace(type, "shintoviz")
      } else {
        stop(paste("plot_type not in ", paste(internal_custom_plot_types, collapse= " ,")))
      }
    }

    sett$data <- plot_data()

    # Make the plot using the settings list
    p <- do.call(plot_fn, sett)


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
  output$tab_data <- shiny::renderTable({

      table_format(plot_data())

  } , digits = 0, align = "l")



  # make exportable
  shiny::callModule(exportButton, "btn_download", data = plot_data)


}



