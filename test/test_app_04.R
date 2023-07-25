
#- Test app 4

# !!! Important !!!
# Run the app with Ctrl-A, Ctrl-Enter (select all, execute), not the 'Run App' button in Rstudio
# Reason: 'my_plot_fun1' has to be in the global environment


# - Reads plot settings from test_app_04.yml
# - Uses table_prepare settings
# - Uses global filter sent to both plots

#- Dependencies
library(shiny)
library(softui)
library(tidyr)
library(dplyr)
library(gapminder)

#- Run example app

devtools::load_all()


plot_data <- gapminder %>%
  mutate(population = pop * 1E-06)

cfg <- yaml::read_yaml("test/test_app_04.yml")


ui <- softui::simple_page(

  softui::box(width = 6,
    numericInput("num1", "Base size", value = 14),
    sliderInput("slide_yr", "Year", value = c(1952, 2007), min = 1952, max = 2007,sep = "")
  ),

  tags$div(id = "placeholder", style = "width: 600px;")


)

server <- function(input, output, session) {

  plot_data_filtered <- reactive({

    yr <- input$slide_yr
    req(yr)

    dplyr::filter(plot_data, between(year, yr[1], yr[2]))


  })

  insert_plot_widgets(data = plot_data_filtered,
                      cfg = cfg,
                      id = "placeholder",
                      plotOutput_only = FALSE,
                      global_settings = reactive(list(
                        base_size = input$num1
                      )))


}

shinyApp(ui, server)



