


library(gapminder)
library(dplyr)

devtools::load_all()


# test
set.seed(100)
data <- expand.grid(
  group = LETTERS[1:10],
  fillvar = letters[1:5]
) %>%
  mutate(value = sample(10000:12000,nrow(.),replace=T))


plot_grouped_horizontal_bars(data,
                             xvar = "group",
                             yvar = "value",
                             label_k = "auto",
                             fillvar = "fillvar",
                             palette_function = "parula")


# deurne
plotdata <- readRDS("c:/repos/wbm3.0/plotdata.rds")

shintoviz::plot_grouped_horizontal_barplot(plotdata,
                                           title = "Test",
                                           yvar = "n_woning",
                                           groupvar = "wijk_naam",
                                           fillvar = "prijsklasse",
                                           #bar_width = input$num_bar_height,
                                           label_k = "auto",
                                           #label_size = as.numeric(input$txt_barlabel_size),
                                           #base_size = as.numeric(input$txt_label_size),
                                           #sort = input$rad_sort_data == "Ja",
                                           #top_n = input$num_show_top_n,
                                           palette_function = "parula")
