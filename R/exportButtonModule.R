
#' @importFrom softui action_button
#' @importFrom shiny downloadButton tags modalDialog NS
#' @importFrom utils write.csv
data_export_modal <- function(ns = shiny::NS(NULL),
                              title = "Export data",
                              info_text = "Kies het gewenste formaat:",
                              formats = c("Excel","CSV","JSON")){

  softui::modal(title = title, icon = bsicon("cloud-download"),confirm_button = FALSE, close_button = TRUE,
                close_txt = "Sluiten",
                shiny::tags$p(info_text),

    if("Excel" %in% formats){
      shiny::downloadButton(ns("btn_excel"), "Excel", icon = bsicon("file-earmark-excel-fill"),
                     class = "bg-gradient-secondary")
    },

    if("CSV" %in% formats){
      shiny::downloadButton(ns("btn_csv"), "CSV", icon = bsicon("filetype-csv"),
                     class = "bg-gradient-secondary")
    },

    if("JSON" %in% formats){
      shiny::downloadButton(ns("btn_json"), "JSON", icon = bsicon("file-code-fill"),
                     class = "bg-gradient-secondary")
    }

  )

}



exportButtonUI <- function(id, export_button_status = "secondary", label = "Export..."){

  ns <- NS(id)

  softui::action_button(ns("btn_export"), label, status = export_button_status)

}

#' @importFrom writexl write_xlsx
#' @importFrom jsonlite toJSON
#' @importFrom shiny showModal observeEvent downloadHandler
exportButton <- function(input, output, session,
                         data,
                         formats = c("Excel", "CSV"),
                         filename_prefix = "data_download"){


  shiny::observeEvent(input$btn_export, {

    shiny::showModal(
      # zie modal.R
      data_export_modal(ns = session$ns,
                        formats = formats)
    )

  })

  output$btn_excel <- shiny::downloadHandler(
    filename = function() {
      paste0(filename_prefix, "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(data(), file)
    }
  )

  output$btn_csv <- shiny::downloadHandler(
    filename = function() {
      paste0(filename_prefix, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      utils::write.csv(data(), file)
    }
  )

  output$btn_json <- shiny::downloadHandler(
    filename = function() {
      paste0(filename_prefix, "_", Sys.Date(), ".json")
    },
    content = function(file) {
      writeLines(jsonlite::toJSON(data(), pretty = TRUE), file)
    }
  )


}

