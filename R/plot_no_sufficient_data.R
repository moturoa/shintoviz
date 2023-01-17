



plot_not_sufficient_data <- function(label = "Niet genoeg data.",
                                     size = 5){

  ggplot2::ggplot(data.frame(x = 50, y= 50, label = label),
                  ggplot2::aes(x = x, y = y, label = label)) +
    ggplot2::theme_void() +
    ggplot2::ylim(c(0,100)) +
    ggplot2::xlim(c(0,100)) +
    ggplot2::geom_text(size = size)

}

#plot_not_sufficient_data()
