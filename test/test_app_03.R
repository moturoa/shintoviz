

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

plot_config_left <- list(
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
      reverse_order = FALSE,
      palette_function = "ocean.phase",
      colors = NULL,
      base_size = 14,
      label_size = 6,
      label_k = FALSE,
      label_hjust = -0.2,
      bar_width = 0.6,
      title = "Ordered"
    )
 )
plot_config_mid <- list(
  list(
    plot_type="plot_value_by_time",
    xvar = "year",
    yvar = "population",
    palette_function = "parula",
    title = "ddt"
  ),
  list(
    plot_type="plot_grouped_value_by_time",
    xvar = "year",
    sub_type='lines',
    yvar = "population",
    group = "continent",
    palette_function = "parula",
    title = "ddt1"
  ),
  list(
    plot_type="plot_grouped_value_by_time",
    xvar = "year",
    sub_type='stacked_bars',
    yvar = "population",
    group = "continent",
    palette_function = "parula",
    title = "ddt2"
  ),
  list(
    plot_type="plot_pie_chart",
    xvar = "continent",
    yvar = "population",
    palette_function = "parula",
    title = "pie"
  ),
  list(
    plot_type="plot_pie_chart",
    xvar = "continent",
    yvar = "population",
    palette_function = "parula",
    title = "pie2"
  )
)



milion_label_format <- function(x,...){
  paste0(round(x * 10E-6,  1)," M.")
}

plot_config_right <- list(

  list(
    title = "Populatie (miljoenen)",
    plot_type = "plot_horizontal_bars",

    table_prepare = list(
      fun = "prepare_grouped_data",
      yvar = "pop",
      groupvar = "continent",
      groupfun = "sum",
      top_n = 10,
      sort = TRUE,
      reverse = TRUE
    ),

    reverse_order = TRUE,
    palette_function = "parula",
    colors = NULL,
    base_size = 14,

    label_function = "milion_label_format",
    label_size = 6,
    label_hjust = -0.2,
    bar_width = 0.6
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


  my_table_format_fun1 <- function(data){
    data$`%` <- round(100 * data[[2]] / sum(data[[2]]), 2)
    data
  }

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
      group_by(continent) %>%
      summarize(population = floor(1e-06 * sum(pop)), .groups = "drop")
  })

  plot_data3 <- reactive({
    gapminder %>%
      mutate(continent = as.character(continent)) %>%
      filter(year == 2007)
  })

  # insert_plot_widgets(data = plot_data2,
  #                     cfg = plot_config_left,
  #                     id = "plot_placeholder_left",
  #                     width = 12)
  # insert_plot_widgets(data = plot_data,
  #                     cfg = plot_config_mid,
  #                     id = "plot_placeholder_mid",
  #                     width = 12)
  insert_plot_widgets(data = plot_data3,
                      cfg = plot_config_right,
                      id = "plot_placeholder_right",
                      width = 12)



}

shinyApp(ui, server)


