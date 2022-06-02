

library(gapminder)
library(dplyr)

par(cex=5)
tab <- filter(gapminder, year == 2007) %>%
  group_by(continent) %>%
  summarize(pop = sum(pop *1e-06), .groups = "drop") %>%
  mutate(label = as.character(pop),
         label_width = strwidth(label, units = "figure"))

xlim <- c(0, max(tab$pop) + max(tab$label_width)*max(tab$pop))


ggplot(tab, aes(x=continent, y=pop)) +
  geom_bar(stat= "identity")  +
  coord_flip() +
  geom_text(aes(label = label), size=5, hjust = - 0.05) +
  lims(y = xlim)



#https://stackoverflow.com/questions/36319229/ggplot2-geom-text-resize-with-the-plot-and-force-fit-text-within-geom-bar

ggplot(tab, aes(x=continent, y=pop)) +
  geom_bar(stat= "identity") +
  coord_flip() +
  geom_text_repel(aes(label = label), size=5, direction = "x")
