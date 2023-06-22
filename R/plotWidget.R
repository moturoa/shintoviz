


#---- Utils ------





#----- UI function ------

#' Shiny UI function for plotWidget
#' @param id Shiny input ID
#' @param header_ui Further UI to be placed above the plot
#' @param footer_ui Further UI to be placed below the plot
#' @param ui_container Either `tab_box` or `tabset_panel` to contain the plot widget
#' @param height Heihgt of the `plotOutput` and `tableOutput`
#' @param \dots Further arguments to [softui::tab_box()]
#' @rdname plotWidget
#' @export
plotWidgetUI <- function(id, header_ui = NULL, footer_ui = NULL,
                         ui_container = c("tab_box","tabset_panel"),

                         height = 400,
                         interactive = NULL,
                         export = FALSE,

                         settingsUI=NULL,

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
                            shiny::plotOutput(ns("plot_main"), height = shiny::validateCssUnit(height)),
                            footer_ui
          ),

          softui::tab_panel(title = softui::bsicon("table"),
                            softui::fluid_row(
                              tags$div(style = paste("height:", shiny::validateCssUnit(height)),

                                       tags$div(style = htmltools::css(height = paste0(height-40, "px"),
                                                                       overflow = "auto", `margin-bottom` =  "5px"),
                                                tableOutput(ns("tab_data"))
                                       ),
                                       exportButtonUI(ns("btn_download"))
                              )
                            )
          ),

          if(!is.null(interactive)){

            softui::tab_panel(title = softui::bsicon("three-dots"),

                              uiOutput(ns("ui_interactive_settings"))

            )

          },

          if(export){
            softui::tab_panel(
              title = softui::bsicon("cloud-download-fill"),

              tags$p("Kies een afmeting, en download de plot als PNG."),

              softui::fluid_row(
                column(6,
                       numericInput(ns("num_png_width"), "Breedte (px)", value = 800, width = "100%")
                ),
                column(6,
                       numericInput(ns("num_png_height"), "Hoogte (px)", value = 600, width = "100%")
                )
                # column(4,
                #        numericInput(ns("num_png_cex"), "Tekst grootte", value = 1, width = "100%", step = 0.05)
                # )
              ),
              radioButtons(ns("rad_bg"), "Achtergrond", choices = c("Wit" = "white", "Transparant" = "transparent"),
                           inline = TRUE, selected = "white"),

              tags$br(),
              shiny::downloadButton(ns("btn_download_plot"), "Download PNG", status = "success",
                                    icon = bsicon("cloud-download-fill")) %>%
                htmltools::tagAppendAttributes(class = "bg-gradient-success")

            )
          }

          # if(!is.null(settingsUI)){
          #   softui::tab_panel(title = softui::bsicon("gear-fill"),
          #                     softui::fluid_row(
          #                       tags$div(style = "height: 400px;",
          #                         settingsUI
          #                       )
          #                     )
          #   )
          # }


  )


}



#----- Server function ------

#' @param input Shiny input (do not set)
#' @param output Shiny output (do not set)
#' @param session Shiny session (do not set)
#' @param data Reactive dataset used in plotting
#' @param plot_type Reactive plotting function. Can be one of [internal_custom_plot_types()]
#' @param settings Reactive list of parameters passed to the plotting function (and table function)
#' @param global_settings Optional reactive list with settings; these override those in settings().
#' Used in `insert_plot_widgets` to pass settings that apply to all plots at the same time.
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
                             global_settings = reactive(NULL),
                             interactive = NULL,
                             extra_ggplot = reactive(NULL),
                             y_min = NULL
){

  ns <- session$ns

  output$ui_interactive_settings <- renderUI({

    req(interactive)

    int_inputs <- names(interactive)

    lapply(int_inputs, function(nm){

      el <- interactive[[nm]]

      if(!("label" %in% names(el) & "choices" %in% names(el))){
        lab <- NULL
        chc <- el
        inlin <- TRUE
      } else {
        lab <- el$label
        chc <- el$choices
        inlin <- FALSE
      }

      radioButtons(ns(paste0("rad_interactive_", nm)),
                   label = lab,
                   inline = inlin,
                   choices = chc)

    })

  })

  interactive_values <- reactive({

    if(is.null(interactive))return(NULL)

    ids <- paste0("rad_interactive_", names(interactive))

    lapply(ids, function(id){
      input[[id]]
    }) %>% setNames(names(interactive))


  })


  # Make data for plotting
  plot_data <- shiny::reactive({

    req(data())
    sett <- settings()

    if(is.null(sett$filter_function)){
      data <- data()
    } else {
      fun <- base::get(sett$filter_function)
      data <- fun(data())
    }


    if(!is.null(sett$table_prepare)){

      cfg <- sett$table_prepare
      fun <- base::get(cfg$fun)
      cfg$fun <- NULL

      yv <- sett$table_prepare$yvar

      if(is.null(yv) && !is.null(sett$table_prepare$groupfun) && sett$table_prepare$groupfun != "length"){
        stop("table_prepare needs 'yvar' setting (the variable name to be summarized)")
      }
      cfg$yvar <- yv

      cfg$data <- data
      data <- do.call(fun, cfg)
    }


    data


  })

  # Format table for showing
  table_data <- reactive({

    sett <- settings()

    if(is.null(sett$table_format)){
      x <- plot_data()
    } else {
      cfg <- sett$table_format
      fun <- base::get(cfg$fun)
      cfg$fun <- NULL
      cfg$data <- plot_data()
      x <- do.call(fun, cfg)
    }

    x
  })


  plot_object <- reactive({

    sett <- settings()

    # check settings, fill with default values
    #settings <- validate_plot_settings(settings)
    template <- sett$template

    # Override plot-level settings with "global_settings" passed to plotmodule or insert_plot_widgets
    globs <- global_settings()
    if(!is.null(globs)){

      for(val in names(globs)){
        sett[[val]] <- globs[[val]]
      }

    }


    # Bypass bijna alles in shintoviz en gebruik een 'custom_function' om de boel te plotten
    if(!is.null(sett$custom_function)){

      plot_fn <- base::get(sett$custom_function$plot)

    } else {

      # Read plot type from settings; or from direct reactive argument
      if("plot_type" %in% names(settings())){
        type <- settings()[["plot_type"]]
      } else {
        type <- plot_type()
      }

      # OR: read from interactive setting
      intvals <- interactive_values()
      if(!is.null(intvals)){
        for(val in names(intvals)){

          sett[[val]] <- intvals[[val]]

          # type is also saved separatly so we can check if the function exists (below)
          if(val == "plot_type"){
            type <- sett[[val]]
          }
        }
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

    # if yvar set in table_group_prepare, set it in main list
    if(!is.null(sett$table_prepare$yvar)){
      sett$yvar <- sett$table_prepare$yvar

      if("yvar" %in% names(intvals)){
        message("When using a `table_prepare`, interactive setting of yvar is not possible.")
      }
    }

    # Read plot data
    sett$data <- plot_data()

    # Make the plot using the settings list
    p <- do.call(plot_fn, sett)


    # Extra adjustments
    # TODO why is this here?
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


  output$plot_main <- shiny::renderPlot({

      plot_object()

  })



  #---- Table

  output$tab_data <- shiny::renderTable({

    table_data()

  } , digits = 1, align = "l")



  # make exportable
  shiny::callModule(exportButton, "btn_download", data = table_data)


  #----- Export
  output$btn_download_plot <- shiny::downloadHandler(

    filename = function(){
      "plot_export_shinto.png"
    },

    content = function(file){

      h <- input$num_png_height
      w <- input$num_png_width
      bg <- input$rad_bg

      req(h)
      req(w)

      p <- plot_object()

      ggplot2::ggsave(p, filename = file, dpi = 72,
                      width = w, height = h,
                      units = "px", bg = bg)

    }

  )

  shiny::outputOptions(output, "ui_interactive_settings", suspendWhenHidden = FALSE)

}
