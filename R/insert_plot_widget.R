
#' Insert plotWidget modules
#' @param data A reactive dataframe, used for all plots
#' @param cfg Config list (maybe read from YAML)
#' @param id Placeholder id where to place plotWidgets (used by insertUI)
#' @param session Shiny session object, no need to set (usually)
#' @param \dots Further arguments passed to `plotWidgetUI`
#' @importFrom uuid UUIDgenerate
#' @importFrom shiny insertUI getDefaultReactiveDomain
#' @export
insert_plot_widgets <- function(data = shiny::reactive(NULL),
                                cfg, id,
                                session = shiny::getDefaultReactiveDomain(),
                                ...){

  lapply(cfg, function(el){

    id_module <- uuid::UUIDgenerate()
    ui <- plotWidgetUI(session$ns(id_module), ...)

    shiny::insertUI(selector = paste0("#",id), ui = ui, where = "beforeEnd",
                    session = session)

    shiny::callModule(plotWidgetModule, id_module, data = data, settings = reactive(el))

  })

}

