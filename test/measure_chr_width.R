
library(grid)

label <- c("label-one-that-might-overlap-another-label",
           "label-two-that-might-overlap-another-label")

#https://stackoverflow.com/questions/55686910/how-can-i-access-dimensions-of-labels-plotted-by-geom-text-in-ggplot2
measure_size <- function(txt, gp = gpar(), to = "mm") {
  if (grid::is.grob(txt)) {
    grobs <- lapply(seq_along(txt$label), function(i) {
      g <- txt
      # Subset grob per label
      g$label <- g$label[[i]]
      g$gp[]  <- lapply(g$gp, function(x) {x[pmin(i, length(x))]})
      g$rot   <- g$rot[pmin(i, length(g$rot))]
      g
    })
  } else {
    grobs <- lapply(txt, function(t) textGrob(t, gp = gp))
  }

  heights <- do.call(unit.c, lapply(grobs, grobHeight))
  widths  <- do.call(unit.c, lapply(grobs, grobWidth))

  cbind(
    height = convertHeight(heights, to, valueOnly = TRUE),
    width = convertWidth(widths,   to, valueOnly = TRUE)
  )
}


library(ggplot2)
pdata <- data.frame(group = c("A","B"),
                    value = c(10,20),
                    label = c("First label", "A long label"))

ymax <- max(pdata$value)
ymin <- 0
p <- ggplot(pdata, aes(x = group, y = value)) +
  coord_flip() +
  geom_bar(stat = "identity") +
  geom_text(aes(label = label), hjust = -0.1, size = 5) +
  ylim(c(ymin, ymax))

m <- measure_size(pdata$label, to = "npc", gp = gpar(cex=2))

i_max <- which.max(pdata$value)
ch_w <- m[i_max,"width"]

p <- p + ylim(c(ymin, ymax + (ymax-ymin)*ch_w))

p
