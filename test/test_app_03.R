

library(softui)
library(shiny)

library(pals)
library(ggplot2)
library(scales)
library(glue)
library(dplyr)

library(gapminder) # example data

devtools::load_all()

set_plotwidget_font("Open Sans")

# yaml::read_yaml

plot_config_left <- list(
  list(
    title = "Title here",
    xvar = "continent",

    reverse_order = FALSE,
    palette_function = "ocean.phase",
    colors = NULL,
    base_size = 14,
    label_size = 4,
    label_k = FALSE,
    label_hjust = 1.65,
    bar_width = 0.6,
    table_prepare = list(
      yvar = "population",
      fun = "prepare_grouped_data",
      groupvar = "continent",
      groupfun = "sum",
      sort = TRUE),

    interactive = list(
      plot_type = c("Cirkeldiagram" = "plot_pie_chart",
        "Staafdiagram" = "plot_horizontal_bars"
                    )
    )

  )
)




plot_config_mid <- list(
  list(
    title = "new",
    xvar = "time",
    sub_type= "lines",
    yvar = "population",
    group=  "continent",
    reverse_order = TRUE,
    palette_function = "ocean.phase",
    colors = NULL,
    base_size = 14,
    label_size = 6,
    label_k = FALSE,
    label_hjust = -0.2,
    bar_width = 4,
    plot_type = "plot_grouped_value_by_time",
    interactive = list(

      sub_type = c("Lijn" = "lines",
                   "Staaf" = "grouped_bars",
                   "Stapeling" = "stacked_bars"
      )
    )
  )
 )


ui <- softui::simple_page(

  softui::fluid_row(
    column(4,
      tags$div(id = "plot_placeholder_left")
    ),
    column(4,
           tags$div(id = "plot_placeholder_mid")
    ),
    column(4,
           tags$div(id = "plot_placeholder_right")
           )
  )

)

server <- function(input, output, session) {

  plot_data <- reactive({
    gapminder %>%
      mutate(  continent = as.character(continent)) %>%
      group_by(continent, year) %>%
      summarize(population = floor(1e-06 * sum(pop)), .groups = "drop") %>%
      mutate(time=year)
  })

  plot_data2 <- reactive({
    gapminder %>%
      mutate(continent = as.character(continent)) %>%
      filter(year == 2007) %>%
      mutate(population = 1e-06 * pop)
  })

  plot_data3 <- reactive({
    gapminder %>%
      mutate(continent = as.character(continent)) %>%
      filter(year == 2007)
  })

  insert_plot_widgets(data = plot_data2,
                      cfg = plot_config_left,
                      id = "plot_placeholder_left",
                      width = 12)

  # insert_plot_widgets(data = plot_data,
  #                      cfg = plot_config_mid,
  #                      id = "plot_placeholder_mid",
  #                      width = 12)
  #
  # insert_plot_widgets(data = plot_data3,
  #                     cfg = plot_config_right,
  #                     id = "plot_placeholder_right",
  #                     width = 12)



}

shinyApp(ui, server)


