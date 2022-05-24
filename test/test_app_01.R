
library(softui)
library(shiny)

library(pals)
library(gapminder)
library(ggplot2)
library(scales)
library(glue)
library(dplyr)

library(gapminder)

devtools::load_all()

ui <- softui::simple_page(

  softui::fluid_row(
    column(6,
      customPlotUI("plot")
    )
  )

)

server <- function(input, output, session) {


  plot_data <- reactive({
    gapminder %>%
      filter(year == 2007) %>%
      group_by(continent) %>%
      summarize(population = floor(1e-06 * sum(pop)), .groups = "drop")
  })

  callModule(customPlot, "plot",
             plot_data = plot_data,
             plot_type = reactive("plot_horizontal_bars"),
             settings = list(
               xvar = "continent",
               yvar = "population",
               palette_function = "ocean.phase",
               colors = NULL,
               base_size = 14,
               label_size = 6,
               label_k = FALSE,
               bar_width = 0.6,
               title = "Populatie (miljoenen)"
             )
            )


}

shinyApp(ui, server)

