#' Formatter for 'n' label next to bars
format_n2 <- function(n, label_k = FALSE, perc= FALSE){

  p <- round(100 * n / sum(n),0)

  if(label_k){
    out <- paste0(round(n/1000,1), "k")
  } else {
    out <- n
  }

  if(perc){
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
