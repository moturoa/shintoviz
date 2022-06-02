
library(softui)
library(shiny)

library(pals)
library(ggplot2)
library(scales)
library(glue)
library(dplyr)

library(gapminder)

devtools::load_all()
#library(shintoviz)


set_plotwidget_font("Maven Pro")


ui <- softui::simple_page(

  softui::fluid_row(
    column(4,
      plotWidgetUI("plot",

                   header_ui = shintoshiny::select_input("sel_continent", NULL,
                                           choices = unique(gapminder$continent),
                                           selected = unique(gapminder$continent),
                                           multiple = TRUE),
                   footer_ui = tagList(
                     numericInput("num_hjust", "Label hjust", value = -0.06),
                     numericInput("num_labelsize", "Label size", value = 4),
                     numericInput("num_barwidth", "Bar width", value = 0.62),
                     numericInput("num_basesize", "Base size", value = 14)
                   )
                   )
    )
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

  plot_data_filtered <- reactive({

    plot_data() %>%
      filter(continent %in% input$sel_continent)

  })

  callModule(plotWidgetModule, "plot",
             plot_data = plot_data_filtered,
             plot_type = reactive("plot_horizontal_bars"),
             settings = reactive(
               list(
                 xvar = "continent",
                 yvar = "population",
                 reverse_order = FALSE,
                 palette_function = "ocean.phase",
                 colors = NULL,
                 base_size = input$num_basesize,
                 label_size = input$num_labelsize,
                 label_k = FALSE,
                 label_perc = TRUE,
                 label_hjust = input$num_hjust,
                 bar_width = input$num_barwidth,
                 title = "Populatie (miljoenen)"
              )
             )
            )


}

shinyApp(ui, server)

