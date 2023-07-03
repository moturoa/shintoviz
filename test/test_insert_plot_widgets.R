
library(shiny)
library(softui)

library(tidyr)
table_komende_jaren_woningtype <- function(data,
                                           next_n_years = 100,
                                           levels = NULL){

  #sthis_year <- lubridate::year(Sys.Date())

  this_year <- min(data$opleverjaar, na.rm = TRUE)
  if(this_year == 0){
    this_year <- lubridate::year(Sys.Date())
  }
  years <- this_year:(this_year + next_n_years)

  out <- data %>%
    mutate(woningtype = na_if(woningtype, ""),
           woningtype = as.factor(woningtype),
           woningtype = forcats::fct_na_value_to_level(woningtype, "Onbekend"))

  if(!is.null(levels)){
    out$woningtype <- factor(out$woningtype, levels = levels)
  }

  out %>%
    dplyr::filter(
      opleverjaar %in% !!years
      #woningtype %in% !!woningtype_filter
    ) %>%
    dplyr::group_by(opleverjaar, woningtype) %>%
    dplyr::summarize(aantalwoningen = sum(aantalwoningen, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::group_by(woningtype) %>%
    dplyr::arrange(opleverjaar) %>%
    dplyr::mutate(aantalwoningen_c = cumsum(aantalwoningen))

}

devtools::load_all()

pdata <- readRDS("c:/repos/wbm3.0/wp.rds") %>%
  filter(opleverjaar > 2023)


#cfg <- yaml::read_yaml("test/testconfig.yml")$config
cfg <- yaml::read_yaml("test/testconfig2.yml")

ui <- softui::simple_page(

  softui::box(width = 4,

    numericInput("num1", "Base size", value = 14)

  ),
  tags$div(id = "placeholder", style = "width: 600px;")


)

server <- function(input, output, session) {

  shintoviz::insert_plot_widgets(reactive(pdata), cfg,
                                 id = "placeholder",
                                 width = 4,
                                 plotOutput_only = TRUE,
                                 global_settings = reactive(list(base_size = input$num1)))


}

shinyApp(ui, server)



