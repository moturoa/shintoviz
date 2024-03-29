
#- Test app 1

# !!! Important !!!
# Run the app with Ctrl-A, Ctrl-Enter (select all, execute), not the 'Run App' button in Rstudio
# Reason: 'my_plot_fun1' has to be in the global environment


#- Dependencies
library(softui)
library(shiny)

library(pals)
library(ggplot2)
library(scales)
library(glue)
library(dplyr)

library(gapminder)

#- Load shintoviz
devtools::load_all()
#library(shintoviz)


#- Run example

set_plotwidget_font("Maven Pro")

my_plot_fun1 <- function(data, year){
  gapminder %>%
    mutate(continent = as.character(continent)) %>%
    filter(year == !!year) %>%
    group_by(continent) %>%
    summarize(population = floor(1e-06 * sum(pop)), .groups = "drop")
}

my_table_format_fun1 <- function(data){
  data$`%` <- round(100 * data[[2]] / sum(data[[2]]), 2)
  data
}

ui <- softui::simple_page(

  softui::fluid_row(
    column(4,
      plotWidgetUI("plot1",

             header_ui = shintoshiny::select_input("sel_continent", NULL,
                                     choices = unique(gapminder$continent),
                                     selected = unique(gapminder$continent),
                                     multiple = TRUE),

             footer_ui = softui::sub_box(collapsed = TRUE, title = "Settings", icon = bsicon("gear-fill"),
                  selectInput("num_year", "Kies jaar data", choices = unique(gapminder$year)),
                 numericInput("num_hjust", "Label hjust", value = -0.19),
                 numericInput("num_labelsize", "Label size", value = 4),
                 numericInput("num_barwidth", "Bar width", value = 0.62),
                 numericInput("num_basesize", "Base size", value = 14)
             )
          )
    ),
    column(4,


      plotWidgetUI("plot2",

             footer_ui = softui::sub_box(collapsed = TRUE, title = "Settings", icon = bsicon("gear-fill"),
               numericInput("num_basesize2", "Base size", value = 14),
               selectInput("sel_plot_type_2", "Type",
                           choices = c("lines","bars")),
               numericInput("num_pointsize", "Point size", value = 3)
             )
      )
    ),
    column(4,

           plotWidgetUI("plot3",

              footer_ui = softui::sub_box(collapsed = TRUE, title = "Settings", icon = bsicon("gear-fill"),
                  numericInput("num_basesize_3", "Base size", value = 14),
                  selectInput("sel_plot_type_3", "Type", choices = c("lines","stacked_bars","grouped_bars")),
                  numericInput("num_pointsize_3", "Point size", value = 3)
              )
           )

    )
  )

)

server <- function(input, output, session) {


  raw_data <- reactive({
    gapminder
  })

  raw_data_filtered <- reactive({

    raw_data() %>%
      filter(continent %in% input$sel_continent)

  })

  callModule(plotWidgetModule, "plot1",
             data = raw_data_filtered,
             plot_type = reactive("plot_horizontal_bars"),
             settings = reactive(
               list(
                 table_prepare = list(
                   fun = "my_plot_fun1",
                   year = input$num_year
                 ),
                 table_format = list(
                   fun = "my_table_format_fun1"
                 ),
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



  plot_data_2 <- reactive({
    gapminder %>%
      filter(country == "Netherlands") %>%
      mutate(pop = round(1e-06* pop,1))
  })

  callModule(plotWidgetModule, "plot2",
             data = plot_data_2,
             plot_type = reactive("plot_value_by_time"),
             settings = reactive(
               list(
                 xvar = "year",
                 yvar = "pop",
                 sub_type = input$sel_plot_type_2,
                 palette_function = NULL,
                 colors = "#33B7A0",
                 base_size = input$num_basesize2,
                 label_size = 2.5,
                 point_size = input$num_pointsize,
                 line_width = 1.2,
                 label_bars = TRUE,
                 label_k = FALSE,
                 ylab = "Populatie",
                 xlab = "Jaar",
                 title = "Nederland"
               )
             )
  )



  plot_data_3 <- reactive({
    gapminder %>%
      filter(country %in% c("Netherlands","Belgium","Luxembourg","Denmark")) %>%
      mutate(pop = 1e-06* pop)
  })

  callModule(plotWidgetModule, "plot3",
             data = plot_data_3,
             plot_type = reactive("plot_grouped_value_by_time"),
             settings = reactive(
               list(
                 xvar = "year",
                 yvar = "pop",
                 group = "country",
                 sub_type = input$sel_plot_type_3,
                 palette_function = "viridis",
                 colors = NULL,
                 base_size = input$num_basesize_3,
                 label_size = 4,
                 point_size = input$num_pointsize_3,
                 line_width = 1.2,
                 label_bars = input$sel_plot_type_3 == "stacked_bars",
                 ylab = "Populatie",
                 xlab = "Jaar",
                 title = "West-Europa"
               )
             )
  )


}

shinyApp(ui, server)

