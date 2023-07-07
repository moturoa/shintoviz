
library(shiny)
library(softui)

library(tidyr)

table_bruto_netto_opleverjaar <- function(data){

  one <- shintoviz::prepare_grouped_data(data, "n_woning_bouw","opleverjaar","sum")
  two <- shintoviz::prepare_grouped_data(data, "n_woning_netto","opleverjaar","sum")

  left_join(one, two, by = "opleverjaar") %>%
    setNames(c("opleverjaar","bruto", "netto")) %>%
    tidyr::pivot_longer(c(bruto, netto), values_to = "aantalwoningen", names_to = "groep")

}


devtools::load_all()

pdata <- readRDS("c:/repos/wbm3.0/wp.rds") %>%
  filter(opleverjaar > 2022)


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
                                 #width = 4,
                                 plotOutput_only = FALSE,
                                 global_settings = reactive(list(base_size = input$num1)))


}

shinyApp(ui, server)



