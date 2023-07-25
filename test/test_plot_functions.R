

# Some simple tests with raw plotting functions
# See 'test_plots.Rmd' for more


library(gapminder)
library(dplyr)

devtools::load_all()


# test
set.seed(100)
data <- expand.grid(
  group = LETTERS[1:10],
  fillvar = letters[1:5]
) %>%
  mutate(value = sample(1000:12000,nrow(.),replace=T))


plot_grouped_horizontal_barplot(data,
                             xvar = "group",
                             yvar = "value",
                             label_k = "auto",
                             fillvar = "fillvar",
                             reverse_palette = FALSE,
                             palette_function = "parula")


data %>%
  mutate(group = as.numeric(group)) %>%
  plot_grouped_value_by_time(.,
                           xvar = "group",
                           yvar = "value",
                           group = "fillvar",
                           plot_type = "stacked_bars",
                           palette_function = "viridis")

