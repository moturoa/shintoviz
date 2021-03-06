


#---- Utils ------





#----- UI function ------

#' Shiny UI function for plotWidget
#' @param id Shiny input ID
#' @param header_ui Further UI to be placed above the plot
#' @param footer_ui Further UI to be placed below the plot
#' @param \dots Further arguments to [softui::tab_box()]
#' @rdname plotWidget
#' @export
plotWidgetUI <- function(id, header_ui = NULL, footer_ui = NULL,
                         ui_container = c("tab_box","tabset_panel"),
                         ...){

  ns <- NS(id)

  ui_container <- match.arg(ui_container)

  ui_fun <- if(ui_container == "tab_box"){
    softui::tab_box
  } else {
    softui::tabset_panel
  }


  ui_fun( style = "margin-top: 10px;", ...,
        softui::tab_panel(title = softui::bsicon("bar-chart-fill"),
                          header_ui,
                          shiny::plotOutput(ns("plot_main")),
                          footer_ui
        ),
        softui::tab_panel(title = softui::bsicon("table"),
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

#' @param input Shiny input (do not set)
#' @param output Shiny output (do not set)
#' @param session Shiny session (do not set)
#' @param data Reactive dataset used in plotting
#' @param plot_type Reactive plotting function. Can be one of [internal_custom_plot_types()]
#' @param settings Reactive list of parameters passed to the plotting function (and table function)
#' @param extra_ggplot A reactive (can be a list) of expressions to add to the ggplot object
#' @param y_min Y-axis minimum value (often 0). TODO include more axis options
#' @rdname plotWidget
#' @importFrom utils getFromNamespace
#' @importFrom shiny plotOutput reactive renderPlot renderTable tableOutput callModule
#' @importFrom softui bsicon tab_panel tab_box fluid_row
#' @importFrom ggplot2 expand_limits
#' @export
#' @examples
#' library(softui)
#' library(gapminder) # example data
#'
#' # Set Google font
#' set_plotwidget_font("Roboto")
#'
#'
#' ui <- softui::simple_page(
#'   plotWidgetUI("plot1", width = 4)
#' )
#'
#' server <- function(input, output, session) {
#'
#'
#'   raw_data <- reactive({
#'     filter(gapminder, year == 2007) %>%
#'       mutate(pop = floor(pop * 1e-06))
#'   })
#'
#'   callModule(plotWidgetModule, "plot1",
#'              data = raw_data,
#'              plot_type = reactive("plot_horizontal_bars"),
#'              settings = reactive(
#'                list(
#'                  table_prepare = list(
#'                    fun = "prepare_grouped_data",
#'                    groupvar = "continent",
#'                    groupfun = sum,
#'                    yvar = "pop"),
#'                  xvar = "continent",
#'                  yvar = "pop",
#'                  palette_function = "parula",
#'                  title = "World population"
#'                )
#'              )
#'   )
#'
#'
#' }
#'
#' #shinyApp(ui, server)
plotWidgetModule <- function(input, output, session,
                       data = reactive(NULL),
                       plot_type = reactive("plot_horizontal_bars"),
                       settings = reactive(list()),
                       extra_ggplot = reactive(NULL),
                       y_min = NULL  # TODO reactive, built-in, in settings?
                       ){

  # observe({
  #   print(session$ns("THISMODULE"))
  #   print(settings())
  # })

  # Make data for plotting
  plot_data <- shiny::reactive({

    sett <- settings()

    if(is.null(sett$table_prepare)){
      data()
    } else {
      cfg <- sett$table_prepare
      fun <- base::get(cfg$fun)
      cfg$fun <- NULL
      cfg$data <- data()
      do.call(fun, cfg)
    }

  })

  # Format table for showing
  table_data <- reactive({

    sett <- settings()

    if(is.null(sett$table_format)){
      plot_data()
    } else {
      cfg <- sett$table_format
      fun <- base::get(cfg$fun)
      cfg$fun <- NULL
      cfg$data <- plot_data()
      do.call(fun, cfg)
    }

  })



  output$plot_main <- shiny::renderPlot({

    sett <- settings()

    # check settings, fill with default values
    #settings <- validate_plot_settings(settings)
    template <- sett$template


    if(!is.null(sett$custom_function)){
      plot_fn <- base::get(sett$custom_function$plot)

    } else {

      if("plot_type" %in% names(settings)){
        type <- settings[["plot_type"]]
      } else {
        type <- plot_type()
      }

      if(type %in% internal_custom_plot_types){
        plot_fn <- utils::getFromNamespace(type, "shintoviz")
      } else {
        stop(paste("plot_type not in ", paste(internal_custom_plot_types, collapse= " ,")))
      }
    }


    # no xvar needed when groupvar present in table_prepare argument
    if(!is.null(sett$table_prepare$groupvar) & is.null(sett$xvar)){
      sett$xvar <- sett$table_prepare$groupvar
    }


    sett$data <- plot_data()

    # Make the plot using the settings list
    p <- do.call(plot_fn, sett)


    # Extra adjustments
    if(!is.null(y_min)){
      p <- p + ggplot2::expand_limits(y = y_min)
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

      table_data()

  } , digits = 1, align = "l")



  # make exportable
  shiny::callModule(exportButton, "btn_download", data = table_data)


}



