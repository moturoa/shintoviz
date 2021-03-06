% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/insert_plot_widget.R
\name{insert_plot_widgets}
\alias{insert_plot_widgets}
\title{Insert plotWidget modules}
\usage{
insert_plot_widgets(
  data = shiny::reactive(NULL),
  cfg,
  id,
  session = shiny::getDefaultReactiveDomain(),
  ...
)
}
\arguments{
\item{data}{A reactive dataframe, used for all plots}

\item{cfg}{Config list (maybe read from YAML)}

\item{id}{Placeholder id where to place plotWidgets (used by insertUI)}

\item{session}{Shiny session object, no need to set (usually)}

\item{\dots}{Further arguments passed to \code{plotWidgetUI}}
}
\description{
Insert plotWidget modules
}
