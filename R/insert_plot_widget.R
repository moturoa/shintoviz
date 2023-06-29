
#' Insert plotWidget modules
#' @param data A reactive dataframe, used for all plots
#' @param cfg Config list (maybe read from YAML)
#' @param id Placeholder id where to place plotWidgets (used by insertUI)
#' @param session Shiny session object, no need to set.
#' @param global_settings Optional reactive list with settings; these override those in cfg,
#' and can be used to set settings that apply to all plots in a single insert.
#' @param renderTable_args List with components 'digits' and 'align' (not more for now), sent to renderTable
#' @param \dots Further arguments passed to `plotWidgetUI`
#' @importFrom uuid UUIDgenerate
#' @importFrom shiny insertUI getDefaultReactiveDomain
#' @export
insert_plot_widgets <- function(data = shiny::reactive(NULL),
                                cfg,
                                id,
                                session = shiny::getDefaultReactiveDomain(),
                                global_settings =  reactive(NULL),
                                renderTable_args = list(digits = 1, align = "l"),
                                ...){

  lapply(cfg, function(el){

    id_module <- uuid::UUIDgenerate()
    ui <- plotWidgetUI(session$ns(id_module), interactive = el$interactive, ...)

    shiny::insertUI(selector = paste0("#",id), ui = ui, where = "beforeEnd",
                    session = session)

    shiny::callModule(plotWidgetModule, id_module, data = data,
                      interactive = el$interactive,
                      settings = reactive(el),
                      renderTable_args = renderTable_args,
                      global_settings = global_settings)

  })

}

