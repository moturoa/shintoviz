# Formatter for 'n' label next to bars
format_n2 <- function(n, label_k = FALSE, label_perc = FALSE, digits = 1){

  if(label_k){
    out <- paste0(round(n/1000,1), "k")
  } else {
    out <- round(n, digits)
  }

  if(label_perc){
    p <- round(100 * n / sum(n),0)
    out <- paste0(out, "(",p,"%)")
  }

  out[n == 0] <- ""
  out
}

format_n <- function(n, label_k = FALSE){

  p <- round(100 * n / sum(n),0)

  if(!label_k){
    out <- paste0(n, "(",p,"%)")
  } else {
    out <- paste0(round(n/1000,1), "k (",p,"%)")
  }


  out[n == 0] <- ""
  out
}

# https://stackoverflow.com/questions/75098826/automatically-leave-enough-room-for-a-label-next-to-a-barplot
fit_horizontal_bar_labels <- function(p) {
  tl <- which(sapply(p$layers, function(x) any(grepl("Text", class(x$geom)))))
  g <- ggplot2::ggplot_build(p)
  range <- g$layout$panel_params[[1]]$x.range
  dat <- g$data[[tl]][order(factor(g$data[[tl]]$label)),]
  label_pos <- dat$x
  labels <- dat$label

  str_width <- sapply(labels, function(x) {
    grid::textGrob(x, gp = gpar(fontsize = p$layers[[tl]]$aes_params$size * ggplot2::.pt)) |>
      grid::grobWidth() |>
      grid::convertWidth("cm", TRUE)
  })

  panel_width <- (unit(1, 'npc') - sum(ggplot2::ggplotGrob(p)$widths[-5])) |>
    grid::convertWidth('cm', TRUE)

  units_per_cm <- diff(range) / panel_width

  new_x <- str_width * units_per_cm + label_pos

  expansion_factor <- (max(new_x) - min(range))/diff(range)
  xval <- expansion_factor^2 * (max(new_x) - max(label_pos)) + max(label_pos)

  p + xlim(NA, xval)
}


