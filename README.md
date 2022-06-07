![](https://badgen.net/badge/shintolabs/utility/purple)

# shintoviz

R package for generic plotting methods with ggplot2, and shiny modules.

## Description

Make configurable consistent plots for Shinto applications with ease.

Includes custom plot functions that make a static images (like `ggplot2` but not `plotly`),
shiny modules for dashboarding ("plot widgets"), and functions to automate many plot widgets based on configuration files.


## Usage

The `shintoviz` package can be used either to make static plots in any medium (a markdown document, a script, 
or a shiny application), or to make opinionated plot widgets for inclusion in shinto applications.

The first step - which is still under development - is to prepare the data for plotting. 

For now, the user has to prepare the data beforehand. For a simple bar chart, data typically need to be summarized in a single metric per group level. Write your own methods to make these tables for plotting. Take a close look at the examples to see what the data should look like for a given custom plot.

In the examples below I use the following example data:

```
library(gapminder)
library(dplyr)

plot_data <- gapminder %>%
 filter(year == 2007) %>%
 group_by(continent) %>%
 summarize(population = floor(1e-06 * sum(pop)), .groups = "drop")
```


1. To prepare for shiny plot widgets, first get used to making a static plot. The example from `?plot_horizontal_bars` :

```
plot_horizontal_bars(plot_data,
                    xvar = "continent",
                    yvar = "population",
                    palette_function = "ocean.phase",
                    colors = NULL,
                    base_size = 14,
                    label_size = 4,
                    label_k = FALSE,
                    bar_width = 0.62,
                    title = "Populatie (miljoenen)")
```

See `?plot_horizontal_bars` for explanation of the parameters. See `shintoviz::internal_custom_plot_types` for 
a list of plot methods currently included in the package. See below how to add more methods or use your own.
All arguments may differ between custom plot methods, there are no assumptions on which arguments are accepted.

It is useful to know that the above function call can rewritten if the arguments are stored in a list:

```
do.call(plot_horizontal_bars,
    list(data = plot_data,
         xvar = "continent",
         yvar = "population",
         palette_function = "ocean.phase",
         colors = NULL,
         base_size = 14,
         label_size = 4,
         label_k = FALSE,
         bar_width = 0.62,
         title = "Populatie (miljoenen)")
    )

```


2. Next, we can make a plot widget for use in `softui` shiny application. The widget will appear
in a `softui::tab_box`. 

Here is a minimal example:

```
library(softui)
library(shintoviz)
library(pals)
library(gapminder)
library(ggplot2)

# Set the Google font for use in the plots
shintoviz::set_plotwidget_font("Roboto")

ui <- softui::simple_page(
        shintoviz::plotWidgetUI("plot1", width = 4)
      )
    
server <- function(input, output, session) {
  
  
  plot_data <- reactive({
    gapminder %>%
      mutate(continent = as.character(continent)) %>%
      filter(year == 2007) %>%
      group_by(continent) %>%
      summarize(population = floor(1e-06 * sum(pop)), .groups = "drop")
  })
  
  callModule(shintoviz::plotWidgetModule, "plot1",
             plot_data = plot_data,
             plot_type = reactive("plot_horizontal_bars"),
             settings = reactive(
               list(
                 xvar = "continent",
                 yvar = "population",
                 reverse_order = FALSE,
                 palette_function = "ocean.phase",
                 colors = NULL,
                 base_size = 14,
                 label_size = 4,
                 label_k = FALSE,
                 label_perc = TRUE,
                 label_hjust = -0.18,
                 bar_width = 0.62,
                 title = "Populatie (miljoenen)"
               )
             )
  )
}

shinyApp(ui, server)
```


3. The following step is to use a configuration file (or other list of settings) to automate
inserting multiple plots that use the same raw dataset. See `test/test_app_02.R` for a working
minimal example.





## Contact

Remko Duursma
