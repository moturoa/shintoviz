
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
      plotWidgetUI("plot1",

             header_ui = shintoshiny::select_input("sel_continent", NULL,
                                     choices = unique(gapminder$continent),
                                     selected = unique(gapminder$continent),
                                     multiple = TRUE),
             footer_ui = tagList(
               numericInput("num_hjust", "Label hjust", value = -0.19),
               numericInput("num_labelsize", "Label size", value = 4),
               numericInput("num_barwidth", "Bar width", value = 0.62),
               numericInput("num_basesize", "Base size", value = 14)
             )
          )
    ),
    column(4,

           plotWidgetUI("plot2",

              footer_ui = tagList(
                numericInput("num_basesize2", "Base size", value = 14),
                selectInput("sel_plot_type_2", "Type", choices = c("lines","bars")),
                numericInput("num_pointsize", "Point size", value = 3)
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

  callModule(plotWidgetModule, "plot1",
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



  plot_data_2 <- reactive({
    gapminder %>%
      filter(country == "Netherlands") %>%
      mutate(pop = round(1e-06* pop,1))
  })

  callModule(plotWidgetModule, "plot2",
             plot_data = plot_data_2,
             plot_type = reactive("plot_value_by_time"),
             settings = reactive(
               list(
                 xvar = "year",
                 yvar = "pop",
                 plot_type = input$sel_plot_type_2,
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


}

shinyApp(ui, server)

