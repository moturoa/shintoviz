

data_export_modal <- function(ns = NS(NULL),
                              title = "Export data", 
                              info_text = "Kies het gewenste formaat:",
                              formats = c("Excel","CSV","JSON")){
  
  modalDialog(
    easyClose = TRUE,
    tags$h3(title),
    tags$hr(),
    
    tags$p(info_text),
    
    if("Excel" %in% formats){
      downloadButton(ns("btn_excel"), "Excel", icon = bsicon("file-earmark-excel-fill"),
                     class = "bg-gradient-secondary")
    },
    
    if("CSV" %in% formats){
      downloadButton(ns("btn_csv"), "CSV", icon = bsicon("filetype-csv"),
                     class = "bg-gradient-secondary")  
    },
    
    if("JSON" %in% formats){
      downloadButton(ns("btn_json"), "JSON", icon = bsicon("file-code-fill"),
                     class = "bg-gradient-secondary")  
    },
    
    
    # ID van deze button maakt niet uit, 'close' gaat via JS (`data-dismiss`) 
    footer = softui::action_button(ns("xyz"), "Sluiten", 
                          icon = bsicon("x-lg"), 
                          status = "danger",
                          `data-bs-dismiss` = "modal")
  )
  
}



exportButtonUI <- function(id, export_button_status = "secondary", label = "Export..."){
  
  ns <- NS(id)
  
  softui::action_button(ns("btn_export"), label, status = export_button_status)
  
}

exportButton <- function(input, output, session, 
                         data, 
                         formats = c("Excel", "CSV"),
                         filename_prefix = "data_download"){
  
  
  observeEvent(input$btn_export, {
    
    showModal(
      # zie modal.R
      data_export_modal(ns = session$ns, 
                        formats = formats)  
    )
    
  })
  
  output$btn_excel <- downloadHandler(
    filename = function() {
      paste0(filename_prefix, "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(data(), file)
    }
  )
  
  output$btn_csv <- downloadHandler(
    filename = function() {
      paste0(filename_prefix, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(data(), file)
    }
  )
  
  output$btn_json <- downloadHandler(
    filename = function() {
      paste0(filename_prefix, "_", Sys.Date(), ".json")
    },
    content = function(file) {
      writeLines(jsonlite::toJSON(data(), pretty = TRUE), file)
    }
  )
  
  
}

