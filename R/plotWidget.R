


#---- Utils ------





#----- UI function ------

#' Shiny UI function for plotWidget
#' @param id Shiny input ID
#' @param header_ui Further UI to be placed above the plot
#' @param footer_ui Further UI to be placed below the plot
#' @param ui_container Either `tab_box` or `tabset_panel` to contain the plot widget
#' @param plotOutput_only Only make a `plotOutput`, not the entire widget with tabs
#' @param height Height of the `plotOutput` and `tableOutput`
#' @param interactive List with interactive settings used to control the plot, usually used via
#' `insert_plot_widgets`. See examples in Juno.
#' @param open_in_modal_button If TRUE, adds a button
#' @param \dots Further arguments to [softui::tab_box()]
#' @rdname plotWidget
#' @export
plotWidgetUI <- function(id,
                         header_ui = NULL,
                         footer_ui = NULL,
                         ui_container = c("tab_box","tabset_panel"),

                         plotOutput_only = FALSE,

                         height = 400,
                         width = "100%",
                         interactive = NULL,

                         open_in_modal_button = TRUE,

                         ...){

  ns <- NS(id)

  ui_container <- match.arg(ui_container)

  ui_fun <- if(ui_container == "tab_box"){
    softui::tab_box
  } else {
    softui::tabset_panel
  }

  p_height <- shiny::validateCssUnit(height)

  if(plotOutput_only){
    return(
      tagList(
        shiny::plotOutput(ns("plot_main"), height = p_height), #, width = width),

        tags$div(style = "display: none;",
          uiOutput(ns("ui_interactive_settings"))
        )

      )

    )
  }

  ui_fun( style = "margin-top: 10px;", ...,
          softui::tab_panel(title = softui::bsicon("bar-chart-fill"),
                            header_ui,
                            shiny::plotOutput(ns("plot_main"), height = p_height), #, width = width),
                            footer_ui
          ),

          softui::tab_panel(title = softui::bsicon("table"),
                            softui::fluid_row(
                              tags$div(style = paste("height:", p_height),

                                       tags$div(style = htmltools::css(height = paste0(height-40, "px"),
                                                                       overflow = "auto", `margin-bottom` =  "5px"),
                                                tableOutput(ns("tab_data"))
                                       ),
                                       exportButtonUI(ns("btn_download"))
                              )
                            )
          ),

          if(open_in_modal_button){

            softui::tab_panel(title = softui::bsicon("display"),

                tags$div(style = paste("height:", p_height),

                   tags$p(bsicon("info-circle-fill", status = "info"),
                            "Pas de verticale afmeting en tekst grootte van de plot aan en klik op de knop."
                          ),

                   # softui::fluid_row(
                   #   column(6,
                            numericInput(ns("num_plot_height"), "Plot hoogte (pixels)", value = 500, min = 0, max = 2000),
                     #),
                     # column(6,
                     #        numericInput(ns("num_plot_width"), "& breedte", value = 1000, min = 0, max = 2000)
                     # )
                   #),

                   numericInput(ns("num_base_size"), "Tekst afmeting",
                          value = 14),

                   softui::action_button(ns("btn_open_in_modal"), "Toon grote versie ...",
                                         icon = bsicon("display"), status = "light")
                )

            )


          },

          if(!is.null(interactive)){

            softui::tab_panel(title = softui::bsicon("three-dots"),

                tags$div(style = paste("height:", p_height),
                  uiOutput(ns("ui_interactive_settings"))
                )

            )

          }



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
#' @param interactive List with interactive controls
#' @param renderTable_args List of arguments (only digits and align) to send to renderTable
#' @param y_min Obsolete
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
                             renderTable_args = list(digits = 1, align = "l"),
                             y_min = NULL){

  ns <- session$ns

  if(!is.null(y_min)){
    warning("Shintoviz: argument y_min in plotWidgetModule is ignored")
  }

  # "Inzoomen" - self module in a modal
  global_settings_for_modal <- reactive({
    req(input$num_base_size)
    glob <- global_settings()
    glob$base_size <- input$num_base_size
    glob
  })

  observeEvent(input$btn_open_in_modal, {

    req(input$num_plot_height)
    #req(input$num_plot_width)

    showModal(
      softui::modal(title = "", icon = bsicon("pie-chart-fill"),
                    size = "xl", close_button = FALSE, confirm_txt = "Sluiten",
                    plotWidgetUI(ns("self_in_modal"),
                                 interactive = interactive,
                                 height = input$num_plot_height,
                                 #width = input$num_plot_width,
                                 open_in_modal = FALSE))
    )

    callModule(plotWidgetModule, "self_in_modal",
               data = data,
               plot_type = plot_type,
               settings = settings,
               global_settings = global_settings_for_modal,
               interactive = interactive,
               extra_ggplot = extra_ggplot,
               renderTable_args)

  })



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

      # OR: read from interactive setting
      intvals <- interactive_values()

      # find settings that are not for table_prepare but for the plot_function
      for_tp <- sapply(sapply(interactive, "[[", "table_prepare"), isTRUE, USE.NAMES = FALSE)
      intvals <- intvals[for_tp]

      if(!is.null(intvals)){
        for(val in names(intvals)){
          cfg[[val]] <- intvals[[val]]
        }
      }

      yv <- sett$table_prepare$yvar

      if(is.null(yv) && !is.null(sett$table_prepare$groupfun) && sett$table_prepare$groupfun != "length"){
        stop("table_prepare needs 'yvar' setting (the variable name to be summarized)")
      }
      cfg$yvar <- yv

      cfg$data <- data

      # maybe more settings from the config
      p <- cfg$params
      cfg$params <- NULL
      cfg <- c(cfg, p)

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

      # Fix some easy to forget settings.
      if(type == "plot_grouped_value_by_time"){
        if(is.null(sett$group) && !is.null(intvals$groupvar)){
          sett$group <- intvals$groupvar
        }
      }

      if(type == "plot_horizontal_bars"){
        if(is.null(sett$groupvar) && !is.null(intvals$groupvar)){
          sett$xvar <- intvals$groupvar
        }
      }



      # find settings that are not for table_prepare but for the plot_function
      for_tp <- sapply(sapply(interactive, "[[", "table_prepare"),isTRUE,USE.NAMES = FALSE)
      intvals <- intvals[!for_tp]

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


    # # Extra adjustments
    # # TODO why is this here?
    # if(!is.null(y_min)){
    #   p <- p + ggplot2::expand_limits(y = y_min)
    # }

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
  tab_arg_lis <- c(list(expr = t))
  output$tab_data <- shiny::renderTable({

    table_data()

  } , digits = renderTable_args$digits, align = renderTable_args$align)


  # make exportable
  shiny::callModule(exportButton, "btn_download", data = table_data)


  # Make sure the interactive settings are available even if not visible
  shiny::outputOptions(output, "ui_interactive_settings", suspendWhenHidden = FALSE)

}
