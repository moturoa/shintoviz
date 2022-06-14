

library(softui)
library(shiny)

library(pals)
library(ggplot2)
library(scales)
library(glue)
library(dplyr)

library(gapminder) # example data

devtools::load_all()

set_plotwidget_font("Roboto")


# yaml::read_yaml

plot_config <- list(
  list(
      xvar = "continent",
      yvar = "population",
      reverse_order = FALSE,
      palette_function = "ocean.phase",
      colors = NULL,
      base_size = 14,
      label_size = 6,
      label_k = FALSE,
      label_hjust = -0.2,
      bar_width = 0.6,
      title = "Title here"
    ),
  list(
    xvar = "continent",
    yvar = "population",
    reverse_order = TRUE,
    palette_function = "parula",
    colors = NULL,
    base_size = 14,
    label_size = 6,
    label_k = FALSE,
    label_hjust = -0.2,
    bar_width = 0.6,
    title = "Populatie (miljoenen)"
  )
)




ui <- softui::simple_page(

  softui::fluid_row(
    tags$div(id = "plot_placeholder")
  )

)

server <- function(input, output, session) {


  plot_data <- reactive({
    gapminder %>%
      mutate(continent = as.character(continent)) %>%
      filter(year == 2007) %>%
      group_by(continent) %>%
      summarize(population = floor(1e-06 * sum(pop)), .groups = "drop")
  })

  insert_plot_widgets(data = plot_data,
                      cfg = plot_config,
                      id = "plot_placeholder",
                      width = 4)


}

shinyApp(ui, server)


