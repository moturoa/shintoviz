#' Formatter for 'n' label next to bars
format_n2 <- function(n, label_k = FALSE, label_perc = FALSE){

  p <- round(100 * n / sum(n),0)

  if(label_k){
    out <- paste0(round(n/1000,1), "k")
  } else {
    out <- n
  }

  if(label_perc){
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



#https://stackoverflow.com/questions/55686910/how-can-i-access-dimensions-of-labels-plotted-by-geom-text-in-ggplot2
geom_text_measure_size <- function(txt, gp = gpar(), to = "mm") {
  if (is.grob(txt)) {
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

